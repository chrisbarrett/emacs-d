;;; +argc.el --- argc-mode: fontify argc directives in shell scripts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that fontifies argc (https://github.com/sigoden/argc)
;; comment directives in shell script buffers and draws box overlays
;; around directive blocks.
;;
;; All visual changes use overlays so they reliably override
;; tree-sitter text properties.

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
                      (looking-at argc--directive-re))
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

(defun argc--block-max-col (beg end)
  "Return the maximum line length between BEG and END."
  (let ((max-col 0))
    (save-excursion
      (goto-char beg)
      (while (<= (point) end)
        (setq max-col (max max-col (- (line-end-position) (line-beginning-position))))
        (when (= (forward-line 1) 1)
          (cl-return))))
    max-col))

(defun argc--make-border (width corner-l corner-r &optional label)
  "Build a box border string of WIDTH chars.
CORNER-L and CORNER-R are the corner characters.
LABEL is an optional right-aligned bold label."
  (let ((face 'argc-box-face))
    (if label
        (let* ((bold-label (propertize label 'face '(:inherit (bold argc-box-face))))
               (label-width (+ (string-width label) 2))
               (fill-width (max 1 (- width 2 label-width)))
               (fill (propertize (make-string fill-width ?─) 'face face)))
          (concat (propertize (string corner-l) 'face face)
                  fill
                  (propertize " " 'face face)
                  bold-label
                  (propertize (concat " " (string corner-r)) 'face face)))
      (let ((fill (propertize (make-string (max 1 (- width 2)) ?─) 'face face)))
        (concat (propertize (string corner-l) 'face face)
                fill
                (propertize (string corner-r) 'face face))))))

(defun argc--apply-box-overlays ()
  "Create box overlays around argc directive blocks."
  (save-excursion
    (dolist (block (argc--find-blocks))
      (let* ((beg (nth 0 block))
             (end (nth 1 block))
             (func-name (argc--function-after end))
             (max-col (argc--block-max-col beg end))
             (box-width (max 80 (+ max-col 4)))
             (face 'argc-box-face)
             (left-border (propertize "│ " 'face face))
             (top (argc--make-border box-width ?┌ ?┐ func-name))
             (bottom (argc--make-border box-width ?└ ?┘)))
        ;; Per-line overlays: content only (lbeg to lend, no newline)
        (goto-char beg)
        (let ((first t))
          (while (<= (point) end)
            (let* ((lbeg (line-beginning-position))
                   (lend (line-end-position))
                   (line-len (- lend lbeg))
                   (pad-len (max 0 (- box-width 4 line-len)))
                   (pad (propertize (make-string pad-len ?\s) 'face face))
                   (right-border (propertize " │" 'face face))
                   (ov (make-overlay lbeg lend)))
              (overlay-put ov 'argc t)
              (overlay-put ov 'argc-box t)
              (if first
                  (let ((top-line (concat top "\n")))
                    (put-text-property 0 (length top-line) 'cursor t top-line)
                    (overlay-put ov 'before-string (concat top-line left-border))
                    (setq first nil))
                (overlay-put ov 'before-string left-border))
              (overlay-put ov 'after-string (concat pad right-border)))
            (when (= (forward-line 1) 1)
              (cl-return))))
        ;; Bottom border: separate overlay after the block
        (let* ((after-pos (min (1+ end) (point-max)))
               (ov (make-overlay after-pos after-pos nil t)))
          (overlay-put ov 'argc t)
          (overlay-put ov 'argc-box t)
          (overlay-put ov 'before-string (concat bottom "\n")))))))

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
  (argc--apply-face-overlays)
  (argc--apply-box-overlays))

(defvar-local argc--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

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
        (advice-add 'spell-fu-mark-incorrect :around #'argc--spell-fu-skip-directives))
    (remove-hook 'after-change-functions #'argc--schedule-rebuild t)
    (advice-remove 'spell-fu-mark-incorrect #'argc--spell-fu-skip-directives)
    (when (timerp argc--rebuild-timer)
      (cancel-timer argc--rebuild-timer))
    (argc--remove-overlays)))

(provide '+argc)

;;; +argc.el ends here
