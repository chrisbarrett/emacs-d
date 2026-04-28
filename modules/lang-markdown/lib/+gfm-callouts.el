;;; +gfm-callouts.el --- Box overlays for GitHub Flavored Markdown callouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that draws box overlays around GitHub Flavored Markdown
;; callout blockquotes:
;;
;;   > [!IMPORTANT]
;;   > Lorem ipsum.
;;
;; The box border picks up the same per-type face used by callout
;; fontification (`+markdown-gfm-callout-*-face').

;;; Code:

(require 'cl-lib)

(defgroup gfm-callouts nil
  "Box overlays for GitHub Flavored Markdown callouts."
  :group 'markdown-faces)

(defface gfm-callouts-box-face
  '((t :inherit shadow))
  "Fallback face for callout box-drawing characters."
  :group 'gfm-callouts)

(defconst gfm-callouts--type-faces
  '(("NOTE"      . +markdown-gfm-callout-note-face)
    ("TIP"       . +markdown-gfm-callout-tip-face)
    ("IMPORTANT" . +markdown-gfm-callout-important-face)
    ("WARNING"   . +markdown-gfm-callout-warning-face)
    ("CAUTION"   . +markdown-gfm-callout-caution-face)
    ("CRITICAL"  . +markdown-gfm-callout-caution-face))
  "Map of callout type to face used for box border and label.")

(defconst gfm-callouts--markup-re
  (rx bol (group ">") " " (group "[") "!"
      (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL")
      (group "]"))
  "Regexp matching marker line markup with positional groups.
Groups: 1=`>', 2=`[', 3=`]'.")

(defconst gfm-callouts--marker-re
  (rx bol "> " "[!"
      (group (or "NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
      "]" (* space) eol)
  "Regexp matching a callout marker line. Group 1 captures the type.")

(defconst gfm-callouts--blockquote-line-re
  (rx bol ">")
  "Regexp matching any blockquote continuation line.")

;;; Block discovery

(defun gfm-callouts--find-blocks ()
  "Return all callout blocks in the current buffer.
Each entry is (BEG END TYPE) where BEG is BOL of the marker line,
END is EOL of the last blockquote line, and TYPE is the callout
type string (e.g. \"IMPORTANT\")."
  (let (blocks)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward gfm-callouts--marker-re nil t)
          (let ((block-beg (line-beginning-position))
                (block-end (line-end-position))
                (type (match-string-no-properties 1)))
            (forward-line 1)
            (while (and (not (eobp))
                        (looking-at gfm-callouts--blockquote-line-re))
              (setq block-end (line-end-position))
              (forward-line 1))
            (push (list block-beg block-end type) blocks)))))
    (nreverse blocks)))

(defun gfm-callouts--block-max-col (beg end)
  "Return the maximum line length between BEG and END."
  (let ((max-col 0))
    (save-excursion
      (goto-char beg)
      (while (<= (point) end)
        (setq max-col (max max-col
                           (- (line-end-position) (line-beginning-position))))
        (when (= (forward-line 1) 1)
          (cl-return))))
    max-col))

;;; Marker-line markup

(defun gfm-callouts--decorate-markup (line-beg)
  "Fade `[' and `]' on the marker line at LINE-BEG against the background."
  (save-excursion
    (save-match-data
      (goto-char line-beg)
      (when (looking-at gfm-callouts--markup-re)
        (let ((quote-face (gfm-callouts--quote-face)))
          (dolist (group '(2 3))
            (let ((ov (make-overlay (match-beginning group) (match-end group))))
              (overlay-put ov 'gfm-callouts t)
              (overlay-put ov 'face quote-face))))))))

(defun gfm-callouts--quote-face ()
  "Face for `>'/`['/`]' markup, blended 10% toward the default foreground."
  (require 'color)
  (let* ((bg (and (boundp '+theme-default-background) +theme-default-background))
         (fg (face-foreground 'default nil t))
         (bg-rgb (and bg (color-name-to-rgb bg)))
         (fg-rgb (and fg (color-name-to-rgb fg))))
    (if (and bg-rgb fg-rgb)
        `(:foreground
          ,(apply #'color-rgb-to-hex
                  (append (cl-mapcar (lambda (b f) (+ b (* 0.1 (- f b))))
                                     bg-rgb fg-rgb)
                          '(2))))
      'shadow)))

;;; Box drawing

(defun gfm-callouts--make-border (width corner-l corner-r border-face)
  "Build a horizontal box border of WIDTH chars using BORDER-FACE.
CORNER-L and CORNER-R are the corner characters."
  (let ((fill (propertize (make-string (max 1 (- width 2)) ?─) 'face border-face)))
    (concat (propertize (string corner-l) 'face border-face)
            fill
            (propertize (string corner-r) 'face border-face))))

(defun gfm-callouts--apply-box-overlays ()
  "Create box overlays around each GFM callout block.

Layout per row (cols 0..box-width inclusive):

  col 0       : buffer `>' (faded to bg colour, outside box)
  col 1       : box left edge — `┌'/`│'/`└'
  cols 2..N-1 : content (label, body, or fill dashes)
  col box-width: box right edge — `┐'/`│'/`┘'

Bottom border lives on its own row, prefixed with one space so its
corners line up with the box edges above."
  (save-excursion
    (dolist (block (gfm-callouts--find-blocks))
      (let* ((beg (nth 0 block))
             (end (nth 1 block))
             (type (nth 2 block))
             (type-face (alist-get type gfm-callouts--type-faces
                                   nil nil #'string=))
             (border-face (or type-face 'gfm-callouts-box-face))
             (quote-face (gfm-callouts--quote-face))
             (max-col (gfm-callouts--block-max-col beg end))
             (box-width (max 80 (+ max-col 2)))
             (body-align-col (1- box-width))
             (right-pipe (propertize "│" 'face border-face))
             (bottom (gfm-callouts--make-border box-width ?└ ?┘ border-face)))
        (goto-char beg)
        (let ((first t)
              last-ov)
          (while (<= (point) end)
            (let* ((lbeg (line-beginning-position))
                   (lend (line-end-position)))
              ;; Fade `>' at col 0 against the background.
              (when (and (> lend lbeg)
                         (eq (char-after lbeg) ?>))
                (let ((ov (make-overlay lbeg (1+ lbeg))))
                  (overlay-put ov 'gfm-callouts t)
                  (overlay-put ov 'gfm-callouts-box t)
                  (overlay-put ov 'face quote-face)))
              ;; Replace the space at col 1 with the box's left edge.
              (when (and (>= (- lend lbeg) 2)
                         (eq (char-after (1+ lbeg)) ?\s))
                (let* ((edge (if first
                                 (propertize "┌─" 'face border-face)
                               (propertize "│ " 'face border-face)))
                       (ov (make-overlay (1+ lbeg) (+ 2 lbeg))))
                  (overlay-put ov 'gfm-callouts t)
                  (overlay-put ov 'gfm-callouts-box t)
                  (overlay-put ov 'display edge)))
              ;; Right edge: marker uses dashes + `┐'; body uses pad + `│'.
              (let* ((after
                      (if first
                          (let* ((label-width
                                  (string-width
                                   (buffer-substring-no-properties
                                    (min lend (+ 2 lbeg)) lend)))
                                 (visual-end (+ 3 label-width))
                                 (dash-count (max 1 (- box-width visual-end))))
                            (concat (propertize (make-string dash-count ?─)
                                                'face border-face)
                                    (propertize "┐" 'face border-face)))
                        (concat (propertize " "
                                            'display `(space :align-to ,body-align-col)
                                            'face border-face)
                                (propertize " " 'face border-face)
                                right-pipe)))
                     (ov (make-overlay lbeg lend nil nil t)))
                (overlay-put ov 'gfm-callouts t)
                (overlay-put ov 'gfm-callouts-box t)
                (put-text-property 0 1 'cursor t after)
                (overlay-put ov 'after-string after)
                (setq last-ov ov))
              (setq first nil))
            (when (= (forward-line 1) 1)
              (cl-return)))
          ;; Bottom border on its own line, prefixed with one space so the
          ;; corners line up under the box edges above.
          (when last-ov
            (let* ((existing (overlay-get last-ov 'after-string))
                   (new-after (concat existing "\n " bottom)))
              (put-text-property 0 1 'cursor t new-after)
              (overlay-put last-ov 'after-string new-after))))
        (gfm-callouts--decorate-markup beg)))))

;;; Overlay management

(defun gfm-callouts--remove-overlays (&optional beg end)
  "Remove all gfm-callouts overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'gfm-callouts t))

(defun gfm-callouts--rebuild ()
  "Remove and recreate all gfm-callouts overlays."
  (gfm-callouts--remove-overlays)
  (gfm-callouts--apply-box-overlays))

(defvar-local gfm-callouts--rebuild-timer nil
  "Idle timer for debounced overlay rebuilds.")

(defun gfm-callouts--schedule-rebuild (&rest _)
  "Schedule a debounced overlay rebuild.
Skips indirect buffers since base buffer overlays already cover them."
  (unless (buffer-base-buffer)
    (when (timerp gfm-callouts--rebuild-timer)
      (cancel-timer gfm-callouts--rebuild-timer))
    (setq gfm-callouts--rebuild-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when gfm-callouts-mode
                   (gfm-callouts--rebuild)))))
           (current-buffer)))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-callouts-mode
  "Render GFM callout blockquotes as boxes."
  :lighter " gfm-cb"
  (if gfm-callouts-mode
      (progn
        (gfm-callouts--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-callouts--schedule-rebuild nil t))
    (remove-hook 'after-change-functions
                 #'gfm-callouts--schedule-rebuild t)
    (when (timerp gfm-callouts--rebuild-timer)
      (cancel-timer gfm-callouts--rebuild-timer))
    (gfm-callouts--remove-overlays)))

(provide '+gfm-callouts)

;;; +gfm-callouts.el ends here
