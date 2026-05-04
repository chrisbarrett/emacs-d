;;; +gfm-callouts.el --- Box overlays for GitHub Flavored Markdown callouts -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that draws box overlays around GitHub Flavored Markdown
;; callout blockquotes:
;;
;;   > [!IMPORTANT]
;;   > Lorem ipsum.
;;
;; The marker line is replaced by an evaporative, cursor-revealable
;; display overlay that integrates the type label into the top border
;; — `┌─ IMPORTANT ─...─┐'.  Each body line's `> ' prefix is replaced
;; by a `│ ' display overlay using the same evaporative + revealable
;; treatment, so editing through the prefix collapses the overlay and
;; moving point in reveals the underlying source.  The right edge is
;; drawn via an after-string aligned to the box width; the bottom
;; border lives on its own visual row beneath the last body line.
;; Border colour follows the per-type face from
;; `gfm-callouts--type-faces'.

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

;;; Box drawing

(defun gfm-callouts--upright (str face)
  "Propertize STR with FACE forced upright.
The display string sits over buffer text whose face may carry
`:slant italic' (e.g. `markdown-blockquote-face'); unspecified
attributes leak from the underlying face, so anchor `:slant normal'."
  (propertize str 'face `(:inherit ,face :slant normal)))

(defun gfm-callouts--top-strings (width title face buffer-width)
  "Build (LEADING . TRAILING) for the top border.

Layout: `┌─ TITLE ─...─┐'.  WIDTH is the total visual width, TITLE
the type label, FACE the border colour, BUFFER-WIDTH the marker
line's buffer-character count.  LEADING covers exactly BUFFER-WIDTH
columns so it can be set as the marker overlay's `display' (matching
the buffer footprint); TRAILING is hung off the line-end as an
after-string and continues to draw the border when reveal exposes
the source."
  (let* ((title-w (string-width title))
         (dash-fill (max 1 (- width 5 title-w)))
         (full (concat
                (gfm-callouts--upright "┌─ " face)
                (gfm-callouts--upright title face)
                (gfm-callouts--upright " " face)
                (gfm-callouts--upright (make-string dash-fill ?─) face)
                (gfm-callouts--upright "┐" face)))
         (full-len (length full))
         (split-at (min buffer-width full-len)))
    (cons (substring full 0 split-at)
          (substring full split-at))))

(defun gfm-callouts--bottom-string (width face)
  "Build the bottom border string of WIDTH cols."
  (concat (gfm-callouts--upright "└" face)
          (gfm-callouts--upright (make-string (max 1 (- width 2)) ?─) face)
          (gfm-callouts--upright "┘" face)))

;;; Overlay registry

(defvar-local gfm-callouts--overlays nil
  "All callout overlays currently in this buffer.")

(defun gfm-callouts--register (ov)
  "Tag OV as a callout overlay and remember it for bulk cleanup."
  (overlay-put ov 'gfm-callouts t)
  (push ov gfm-callouts--overlays)
  ov)

(defun gfm-callouts--apply-marker-line (beg border-face top-split)
  "Adorn the marker line at BEG.
The marker line gets a whole-line evaporative + revealable display
overlay carrying (car TOP-SPLIT); a 0-width overlay at the line's end
hosts (cdr TOP-SPLIT) as an after-string, which is later extended with
the bottom border.  Returns the trailing after-string overlay so
subsequent body lines (if any) can move the bottom border onto them."
  (let ((line-end (save-excursion
                    (goto-char beg) (line-end-position))))
    (let ((ov (make-overlay beg line-end)))
      (overlay-put ov 'gfm-callouts-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display (car top-split))
      (gfm-callouts--register ov))
    (let ((ov (make-overlay line-end line-end nil nil t)))
      (overlay-put ov 'after-string (cdr top-split))
      (gfm-callouts--register ov)
      ov)))

(defun gfm-callouts--apply-body-line (lbeg lend edge body-align-col
                                           right-pipe border-face)
  "Adorn one body line and return its right-edge overlay."
  (when (and (>= (- lend lbeg) 2)
             (eq (char-after lbeg) ?>)
             (eq (char-after (1+ lbeg)) ?\s))
    (let ((ov (make-overlay lbeg (+ 2 lbeg))))
      (overlay-put ov 'gfm-callouts-revealable t)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'display edge)
      (gfm-callouts--register ov)))
  (let* ((after (concat
                 (propertize " "
                             'display `(space :align-to ,body-align-col)
                             'face `(:inherit ,border-face :slant normal))
                 (gfm-callouts--upright " " border-face)
                 right-pipe))
         (ov (make-overlay lbeg lend nil nil t)))
    (put-text-property 0 1 'cursor t after)
    (overlay-put ov 'after-string after)
    (gfm-callouts--register ov)
    ov))

(defun gfm-callouts--apply-box-overlays ()
  "Create box overlays around each GFM callout block.

Marker line: full-line evaporative + revealable display overlay
carrying `┌─ TITLE ─...─┐' (split into a leading display covering the
buffer footprint and a trailing after-string for the rest).  Body
lines: cols 0-1 (`> ') are replaced with a `│ ' display overlay; the
right edge is drawn via after-string aligned to box-width-2.  The
bottom border is appended to the final right-edge after-string on its
own visual row (or to the marker's trailing after-string when the
callout has no body lines)."
  (save-excursion
    (dolist (block (gfm-callouts--find-blocks))
      (let* ((beg (nth 0 block))
             (end (nth 1 block))
             (type (nth 2 block))
             (type-face (alist-get type gfm-callouts--type-faces
                                   nil nil #'string=))
             (border-face (or type-face 'gfm-callouts-box-face))
             (max-col (gfm-callouts--block-max-col beg end))
             (box-width (max 80 (+ max-col 2)))
             ;; `│' should land at col (box-width-1).  After
             ;; `space :align-to N' cursor is at col N, the trailing
             ;; space advances to N+1, then `│' renders at col N+1.
             (body-align-col (- box-width 2))
             (marker-line-end (save-excursion
                                (goto-char beg) (line-end-position)))
             (marker-buf-w (- marker-line-end beg))
             (top-split (gfm-callouts--top-strings box-width type border-face
                                                   marker-buf-w))
             (bottom (gfm-callouts--bottom-string box-width border-face))
             (edge (gfm-callouts--upright "│ " border-face))
             (right-pipe (gfm-callouts--upright "│" border-face))
             (trailing-ov (gfm-callouts--apply-marker-line
                           beg border-face top-split)))
        ;; Override `markdown-blockquote-face' italic across the
        ;; block: callouts are alerts, not quotations.
        (let ((ov (make-overlay beg end)))
          (overlay-put ov 'face '(:slant normal))
          (gfm-callouts--register ov))
        (save-excursion
          (goto-char marker-line-end)
          (forward-line 1)
          (while (and (not (eobp)) (<= (point) end))
            (let* ((lbeg (line-beginning-position))
                   (lend (line-end-position))
                   (ov (gfm-callouts--apply-body-line
                        lbeg lend edge body-align-col right-pipe border-face)))
              (setq trailing-ov ov))
            (forward-line 1)))
        ;; Bottom border on its own visual line, attached to the last
        ;; right-edge after-string (or the marker's trailing piece if
        ;; the callout has no body lines).
        (let* ((existing (overlay-get trailing-ov 'after-string))
               (new-after (concat existing "\n" bottom)))
          (put-text-property 0 1 'cursor t new-after)
          (overlay-put trailing-ov 'after-string new-after))))))

;;; Cursor-driven reveal

(defvar-local gfm-callouts--hidden-ovs nil
  "Revealable overlays whose display is currently suppressed.")

(defun gfm-callouts--reveal ()
  "Suppress display on revealable overlays containing point; restore others."
  (let ((pos (point)))
    (setq gfm-callouts--hidden-ovs
          (cl-loop for ov in gfm-callouts--hidden-ovs
                   if (and (overlay-buffer ov)
                           (>= pos (overlay-start ov))
                           (<= pos (overlay-end ov)))
                   collect ov
                   else do (when (overlay-buffer ov)
                             (overlay-put ov 'display
                                          (overlay-get ov 'gfm-callouts-saved-display))
                             (overlay-put ov 'gfm-callouts-saved-display nil))))
    (dolist (ov (overlays-in pos (1+ pos)))
      (when (and (overlay-get ov 'gfm-callouts-revealable)
                 (overlay-get ov 'display)
                 (not (memq ov gfm-callouts--hidden-ovs)))
        (overlay-put ov 'gfm-callouts-saved-display
                     (overlay-get ov 'display))
        (overlay-put ov 'display nil)
        (push ov gfm-callouts--hidden-ovs)))))

;;; Overlay management

(defun gfm-callouts--remove-overlays (&optional beg end)
  "Remove all gfm-callouts overlays between BEG and END."
  (remove-overlays (or beg (point-min)) (or end (point-max))
                   'gfm-callouts t)
  (unless (or beg end)
    (setq gfm-callouts--overlays nil
          gfm-callouts--hidden-ovs nil)))

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
                  #'gfm-callouts--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-callouts--reveal nil t))
    (remove-hook 'after-change-functions
                 #'gfm-callouts--schedule-rebuild t)
    (remove-hook 'post-command-hook #'gfm-callouts--reveal t)
    (when (timerp gfm-callouts--rebuild-timer)
      (cancel-timer gfm-callouts--rebuild-timer))
    (gfm-callouts--remove-overlays)))

(provide '+gfm-callouts)

;;; +gfm-callouts.el ends here
