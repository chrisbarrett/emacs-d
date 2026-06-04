;;; argc-mode.el --- Fontify argc CLI directives in shell scripts -*- lexical-binding: t; -*-

;;; Commentary:

;; `argc-mode' is a minor mode that fontifies argc
;; (https://github.com/sigoden/argc) comment directives in shell
;; script buffers and draws Unicode box overlays around contiguous
;; directive blocks.
;;
;; All visual changes use overlays so they reliably override
;; tree-sitter text properties.
;;
;; Activation is intentionally not handled here; consumers wire the
;; mode up via their own mode hooks (e.g. `sh-mode-hook',
;; `bash-ts-mode-hook') with whatever gate they prefer.

;;; Code:

(require 'cl-lib)

(defgroup argc nil
  "Fontification for argc CLI framework directives."
  :group 'sh-script)

(defface argc-directive-face
  '((t :inherit font-lock-keyword-face))
  "Face for argc directive tags (@cmd, @arg, etc)."
  :group 'argc)

(defface argc-param-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for argc parameter names."
  :group 'argc)

(defface argc-flag-face
  '((t :inherit font-lock-constant-face))
  "Face for argc short/long flags (-f, --force)."
  :group 'argc)

(defface argc-modifier-face
  '((t :inherit font-lock-type-face))
  "Face for argc modifiers (!, *, +)."
  :group 'argc)

(defface argc-notation-face
  '((t :inherit font-lock-type-face))
  "Face for argc value notations (<FILE>, <NUM>)."
  :group 'argc)

(defface argc-choice-face
  '((t :inherit font-lock-type-face))
  "Face for argc choice lists ([a|b|c])."
  :group 'argc)

(defface argc-default-value-face
  '((t :inherit font-lock-string-face))
  "Face for argc default values (=value)."
  :group 'argc)

(defface argc-box-face
  '((t :inherit shadow))
  "Face for argc box-drawing characters."
  :group 'argc)

(defconst argc--directive-re
  (rx bol (* space) "#" (+ space)
      "@" (or "describe" "cmd" "alias" "arg" "option"
              "flag" "env" "meta"))
  "Regexp matching a line containing an argc directive.")

(defconst argc--comment-line-re
  (rx bol (* space) "#")
  "Regexp matching any comment line.")

(defvar argc--face-rules
  `(;; Base: description text gets doc face.
    ;; Later rules create higher-priority overlays that override.
    (,(rx bol (* space) "#" (+ space)
          "@" (or "describe" "cmd" "alias" "arg" "option"
                  "flag" "env" "meta")
          (+ space) (group (+? nonl) eol))
     (1 font-lock-doc-face))

    ;; @directive tag
    (,(rx bol (* space) "#" (+ space)
          (group "@" (or "describe" "cmd" "alias" "arg" "option"
                         "flag" "env" "meta")))
     (1 argc-directive-face))

    ;; @arg <name><modifier>
    (,(rx bol (* space) "#" (+ space) "@arg" (+ space)
          (group (+ (any alnum ?_ ?-)))
          (? (group (any ?! ?* ?+ ?, ?~))))
     (1 argc-param-name-face)
     (2 argc-modifier-face))

    ;; @option short/long flags and name
    (,(rx bol (* space) "#" (+ space) "@option" (+ space)
          (group (? "-" (any alnum) (+ space))
                 "--" (+ (any alnum ?_ ?-)))
          (? (group (any ?! ?* ?+))))
     (1 argc-flag-face)
     (2 argc-modifier-face))

    ;; @flag short/long flags
    (,(rx bol (* space) "#" (+ space) "@flag" (+ space)
          (group (? "-" (any alnum) (+ space))
                 "--" (+ (any alnum ?_ ?-)))
          (? (group (any ?*))))
     (1 argc-flag-face)
     (2 argc-modifier-face))

    ;; @env NAME with modifier
    (,(rx bol (* space) "#" (+ space) "@env" (+ space)
          (group (+ (any upper ?_)))
          (? (group (any ?! ?*))))
     (1 argc-param-name-face)
     (2 argc-modifier-face))

    ;; @meta key value
    (,(rx bol (* space) "#" (+ space) "@meta" (+ space)
          (group (+ (any alnum ?_  ?-))))
     (1 argc-param-name-face))

    ;; @alias names
    (,(rx bol (* space) "#" (+ space) "@alias" (+ space)
          (group (+ (any alnum ?_ ?- ?,))))
     (1 argc-param-name-face))

    ;; Angle-bracket notations <FILE>, <NUM> etc
    (,(rx bol (* space) "#" (+ space) "@" (or "option" "arg" "env") (+ nonl)
          (group "<" (+ (any alnum ?_ ?+ ?* ??)) ">"))
     (1 argc-notation-face))

    ;; Choice lists [a|b|c]
    (,(rx bol (* space) "#" (+ space) "@" (or "option" "arg") (+ nonl)
          (group "[" (+? nonl) "]"))
     (1 argc-choice-face))

    ;; Default values =value (not inside brackets)
    (,(rx bol (* space) "#" (+ space) "@" (or "option" "arg") (+ nonl)
          (group "=" (+ (not (any space ?\[ ?\])))))
     (1 argc-default-value-face)))
  "Argc face rules.
Each entry is (REGEXP (GROUP FACE) ...).  Used by `argc--apply-face-overlays'.
Rules are processed in order; later rules create higher-priority overlays.")

;;; Face overlays

(defun argc--apply-face-overlays ()
  "Create face overlays for argc directives in the current buffer."
  (save-excursion
    (save-match-data
      (let ((priority 0))
        (dolist (rule argc--face-rules)
          (let ((regexp (car rule))
                (groups (cdr rule)))
            (goto-char (point-min))
            (while (re-search-forward regexp nil t)
              (dolist (spec groups)
                (let ((grp (car spec))
                      (face (cadr spec)))
                  (when (match-beginning grp)
                    (let ((ov (make-overlay (match-beginning grp) (match-end grp))))
                      (overlay-put ov 'argc t)
                      (overlay-put ov 'face face)
                      (overlay-put ov 'priority priority)))))))
          (cl-incf priority))))))

;;; Box overlays

(defconst argc--func-re
  (rx bol (* space)
      (or (seq "function" (+ space)
               (group (+ (any alnum ?_ ?-)))
               (* space) (? "()") (* space) "{")
          (seq (group (+ (any alnum ?_ ?-)))
               (* space) "()" (* space) "{")))
  "Regexp matching a shell function definition.
Group 1 or 2 captures the function name.")

(defun argc--find-blocks ()
  "Find all contiguous blocks of argc directive lines.
Return list of (BEG END) where BEG is BOL of first directive
and END is EOL of last directive in each block."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward argc--directive-re nil t)
        (let ((block-beg (line-beginning-position))
              (block-end (line-end-position)))
          (forward-line 1)
          (while (and (not (eobp))
                      (looking-at argc--comment-line-re))
            (setq block-end (line-end-position))
            (forward-line 1))
          (push (list block-beg block-end) blocks))))
    (nreverse blocks)))

(defun argc--function-after (pos)
  "Return function name defined after POS, or nil.
Checks the next non-blank line after POS."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (skip-chars-forward " \t\n")
    (when (looking-at argc--func-re)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)))))

(defun argc--normalised-box-face ()
  "Return a face spec inheriting `argc-box-face' with text styling cleared.
Border glyphs share buffer regions with prose font-lock that may
carry `:slant italic', `:underline t', etc; without an explicit
override those attrs leak through face composition on GUI frames
and slant the box edges.  `:background' is pinned to the system
marker `\"unspecified-bg\"' so text-property backgrounds (e.g.
`diff-added' on a body line) do not bleed into border /
before-string / after-string chars."
  '(:inherit argc-box-face
    :slant normal
    :underline nil :overline nil :strike-through nil :box nil
    :background "unspecified-bg"))

(defun argc--box-width ()
  "Target column count for the top / bottom borders.
Each body line's right rail `│' is positioned at `:align-to (- right
3)' — i.e. 3 columns shy of the text area's right edge.  A border of
this many glyphs spans columns 0..WIDTH-1, landing its right corner on
the same column as that rail so the box closes flush.  Falls back to
the selected window when the buffer is not displayed."
  (let* ((win (get-buffer-window (current-buffer) t))
         (w (if win (window-body-width win) (window-body-width))))
    (max 20 (- w 2))))

(defun argc--make-border (corner-l corner-r width &optional label)
  "Build a WIDTH-column box border whose rule is drawn with `─' glyphs.
CORNER-L and CORNER-R are the corner characters at columns 0 and
WIDTH-1.  LABEL, when non-nil, is drawn in bold right-aligned just
before CORNER-R, the dash fill running up to it.

The fill uses literal `─' rather than a blank `:align-to' stretch
\(which renders no visible rule), so the horizontal edge is continuous
the way the `gfm-pretty' fence borders are."
  (let* ((face (argc--normalised-box-face))
         (left (propertize (string corner-l) 'face face))
         (right-corner (propertize (string corner-r) 'face face)))
    (if label
        (let* ((label-face `(:inherit (bold argc-box-face)
                             :slant normal
                             :underline nil :overline nil
                             :strike-through nil :box nil
                             :background "unspecified-bg"))
               (bold-label (propertize label 'face label-face))
               (label-w (string-width label))
               ;; ┌ + dashes + ' ' + label + ' ' + ┐ = WIDTH
               (dash-w (max 1 (- width 4 label-w)))
               (fill (propertize (make-string dash-w ?─) 'face face))
               (gap (propertize " " 'face face)))
          (concat left fill gap bold-label gap right-corner))
      (let* ((dash-w (max 0 (- width 2)))
             (fill (propertize (make-string dash-w ?─) 'face face)))
        (concat left fill right-corner)))))

(defun argc--substitute-hash (lbeg lend face)
  "Replace the `#' at the start of line [LBEG, LEND] with a `│' overlay.
The overlay is single-char, evaporating, and uses `display' to swap in
`│' painted in FACE.  The substituted glyph is also stashed under the
`argc-hash-display' overlay prop so `argc--reveal-hash-at-point' can
restore it after a transient reveal.  Mirrors the `> ' → `│ '
substitution in `gfm-pretty-callouts--apply-block-display'."
  (save-excursion
    (goto-char lbeg)
    (skip-chars-forward " \t" lend)
    (when (and (< (point) lend) (eq (char-after) ?#))
      (let ((sub (make-overlay (point) (1+ (point))))
            (disp (propertize "│" 'face face)))
        (overlay-put sub 'argc t)
        (overlay-put sub 'argc-box t)
        (overlay-put sub 'argc-hash-sub t)
        (overlay-put sub 'argc-hash-display disp)
        (overlay-put sub 'evaporate t)
        (overlay-put sub 'display disp)))))

(defvar-local argc--revealed-hash nil
  "Substitution overlay whose `display' is currently suppressed for reveal.")

(defun argc--reveal-hash-at-point (&rest _)
  "Reveal the source `#' under point by hiding its substitution overlay.
Run from `post-command-hook'.  Restores the previously revealed overlay
when point moves off; mirrors prettify-symbols-mode's
unprettify-at-point semantics but at the overlay layer so it composes
with the rest of the box overlays."
  (let* ((subs (cl-remove-if-not
                (lambda (ov) (overlay-get ov 'argc-hash-sub))
                (overlays-at (point))))
         (here (car subs)))
    (when (and argc--revealed-hash
               (not (eq argc--revealed-hash here))
               (overlay-buffer argc--revealed-hash))
      (overlay-put argc--revealed-hash 'display
                   (overlay-get argc--revealed-hash 'argc-hash-display)))
    (when (and here (not (eq here argc--revealed-hash)))
      (overlay-put here 'display nil))
    (setq argc--revealed-hash here)))

(defun argc--apply-box-overlays ()
  "Create box overlays around argc directive blocks."
  (save-excursion
    (dolist (block (argc--find-blocks))
      (let* ((beg (nth 0 block))
             (end (nth 1 block))
             (func-name (argc--function-after end))
             (face (argc--normalised-box-face))
             (box-width (argc--box-width))
             (wrap-rail (propertize "│ " 'face face))
             (top (argc--make-border ?┌ ?┐ box-width func-name))
             (bottom (argc--make-border ?└ ?┘ box-width)))
        (goto-char beg)
        (let ((first t)
              (done nil)
              last-ov)
          (while (and (not done) (<= (point) end))
            (let* ((lbeg (line-beginning-position))
                   (lend (line-end-position))
                   (pad (propertize " " 'face face
                                    'display '(space :align-to (- right 3))))
                   (right-border (propertize "│" 'face face))
                   (mask (propertize " " 'face 'default
                                     'display '(space :align-to (- right 1))))
                   (ov (make-overlay lbeg lend nil nil t))
                   (after (concat pad right-border mask)))
              (overlay-put ov 'argc t)
              (overlay-put ov 'argc-box t)
              (overlay-put ov 'wrap-prefix wrap-rail)
              (when first
                (overlay-put ov 'before-string (concat top "\n"))
                (setq first nil))
              (put-text-property 0 1 'cursor t after)
              (overlay-put ov 'after-string after)
              (argc--substitute-hash lbeg lend face)
              (setq last-ov ov))
            (when (= (forward-line 1) 1)
              (setq done t)))
          (when last-ov
            (let* ((pad (propertize " " 'face face
                                    'display '(space :align-to (- right 3))))
                   (right-border (propertize "│" 'face face))
                   (mask (propertize " " 'face 'default
                                     'display '(space :align-to (- right 1))))
                   (after (concat pad right-border mask "\n" bottom)))
              (put-text-property 0 1 'cursor t after)
              (overlay-put last-ov 'after-string after))))))))

;;; Overlay management

(defun argc--remove-overlays (&optional beg end)
  "Remove all argc overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max)) 'argc t))

(defun argc--on-directive-line-p (pos)
  "Return non-nil if POS is on an argc directive line."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (looking-at argc--directive-re)))

(defun argc--spell-fu-skip-directives (orig-fn pos-beg pos-end)
  "Advise `spell-fu-mark-incorrect' to skip argc directive lines.
ORIG-FN is the original function, POS-BEG and POS-END the range."
  (unless (argc--on-directive-line-p pos-beg)
    (funcall orig-fn pos-beg pos-end)))

(defun argc--rebuild ()
  "Remove and recreate all argc overlays."
  (argc--remove-overlays)
  (setq argc--revealed-hash nil)
  (argc--apply-face-overlays)
  (argc--apply-box-overlays))

(defvar-local argc--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

;; Forward declaration; the variable is created by `define-minor-mode' below.
(defvar argc-mode)

(defun argc--schedule-rebuild (&rest _)
  "Schedule a debounced overlay rebuild.
Skips indirect buffers since base buffer overlays are already visible."
  (unless (buffer-base-buffer)
    (when (timerp argc--rebuild-timer)
      (cancel-timer argc--rebuild-timer))
    (setq argc--rebuild-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when argc-mode
                   (argc--rebuild)))))
           (current-buffer)))))

;;; Minor mode

;;;###autoload
(define-minor-mode argc-mode
  "Fontify argc comment directives in shell script buffers."
  :lighter " argc"
  (if argc-mode
      (progn
        (argc--rebuild)
        (add-hook 'after-change-functions #'argc--schedule-rebuild nil t)
        ;; Borders are sized to the window width, so re-fit them when the
        ;; window is resized or reconfigured.
        (add-hook 'window-configuration-change-hook #'argc--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'argc--reveal-hash-at-point nil t)
        (advice-add 'spell-fu-mark-incorrect :around #'argc--spell-fu-skip-directives))
    (remove-hook 'after-change-functions #'argc--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook #'argc--schedule-rebuild t)
    (remove-hook 'post-command-hook #'argc--reveal-hash-at-point t)
    (advice-remove 'spell-fu-mark-incorrect #'argc--spell-fu-skip-directives)
    (when (timerp argc--rebuild-timer)
      (cancel-timer argc--rebuild-timer))
    (setq argc--revealed-hash nil)
    (argc--remove-overlays)))

(provide 'argc-mode)

;;; argc-mode.el ends here
