;;; gfm-present.el --- Slide walkthrough over markdown documents -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local minor-mode walk-through over a markdown document.  Top-
;; level H1 headings are slides; navigation narrows to the heading
;; region containing point.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'gfm-pretty)
(require 'gfm-pretty-borders)

;; Forward declarations for byte-compiler.
(defvar gfm-present-mode nil)
(defvar gfm-present-mode-map)
(defvar gfm-pretty-links-after-anchor-jump-functions)
(declare-function evil-define-key* "evil-core")
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function gfm-pretty-links--jump-to-anchor "gfm-pretty-links" (anchor))
(declare-function magit-diff-range "magit-diff")
(declare-function markdown-follow-link-at-point "markdown-mode")
(declare-function markdown-follow-thing-at-point "markdown-mode")

;;; Faces

(defface gfm-present-focus-face
  '((((class color) (min-colors 89) (background dark))
     :background "grey20")
    (((class color) (min-colors 89) (background light))
     :background "grey90"))
  "Face for focus highlight in narrowed-source previews."
  :group 'gfm-present)

;;; Heading helpers

(defconst gfm-present--fence-rx
  (rx bol (* blank) "```")
  "Regexp matching a markdown fenced-code-block delimiter line.")

(defconst gfm-present--h1-rx
  (rx bol "# " (group (* nonl)) eol)
  "Regexp matching a top-level markdown heading line.")

(defun gfm-present--all-h1-positions ()
  "Return buffer positions of every top-level H1 line.
Lines inside fenced code blocks are excluded.  Positions are line
beginnings, in document order."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((positions nil)
            (in-fence nil))
        (while (not (eobp))
          (cond
           ((looking-at gfm-present--fence-rx)
            (setq in-fence (not in-fence)))
           ((and (not in-fence) (looking-at gfm-present--h1-rx))
            (push (line-beginning-position) positions)))
          (forward-line 1))
        (nreverse positions)))))

(defun gfm-present--heading-region (pos)
  "Return (START . END) of the H1 region containing POS, or nil.
START is the beginning of the H1 line; END is either the beginning
of the next H1 line or `point-max'."
  (let* ((positions (gfm-present--all-h1-positions))
         (preceding (cl-loop for p in positions while (<= p pos) collect p))
         (start (car (last preceding))))
    (when start
      (let* ((rest (cdr (member start positions)))
             (end (or (car rest) (point-max))))
        (cons start end)))))

(defun gfm-present--heading-slug (text)
  "Return TEXT slugified.
Downcase, collapse runs of non-alphanumeric characters to a single
hyphen, then strip leading and trailing hyphens."
  (let* ((s (downcase (or text "")))
         (s (replace-regexp-in-string
             (rx (one-or-more (not (any alnum)))) "-" s))
         (s (replace-regexp-in-string
             (rx (or (: bos (one-or-more "-"))
                     (: (one-or-more "-") eos)))
             "" s)))
    s))

(defun gfm-present--narrow-to-heading-at (pos)
  "Widen, then narrow to the H1 region containing POS.
When POS is before the first H1, narrow to the first H1's region.
When the document has no H1s, leave the buffer widened."
  (widen)
  (let ((region (gfm-present--heading-region pos)))
    (cond
     (region
      (narrow-to-region (car region) (cdr region)))
     (t
      (let ((positions (gfm-present--all-h1-positions)))
        (when positions
          (let* ((first (car positions))
                 (next (cadr positions))
                 (end (or next (point-max))))
            (narrow-to-region first end))))))))

;;; Navigation commands

(defun gfm-present--current-h1-start ()
  "Return the buffer position of the H1 enclosing point, or nil."
  (save-restriction
    (widen)
    (car-safe (gfm-present--heading-region (point)))))

(defun gfm-present-next-slide ()
  "Advance to the next H1 and narrow there.  Silent no-op at last slide."
  (interactive)
  (let* ((positions (save-restriction
                      (widen)
                      (gfm-present--all-h1-positions)))
         (current (gfm-present--current-h1-start))
         (next (if current
                   (cadr (member current positions))
                 (car positions))))
    (when next
      (gfm-present--narrow-to-heading-at next)
      (goto-char (point-min))
      (when gfm-present-mode
        (gfm-present--render-link-previews)))))

(defun gfm-present-previous-slide ()
  "Retreat to the previous H1 and narrow there.  Silent no-op at first."
  (interactive)
  (let* ((positions (save-restriction
                      (widen)
                      (gfm-present--all-h1-positions)))
         (current (gfm-present--current-h1-start))
         (prev (when current
                 (cadr (member current (reverse positions))))))
    (when prev
      (gfm-present--narrow-to-heading-at prev)
      (goto-char (point-min))
      (when gfm-present-mode
        (gfm-present--render-link-previews)))))

;;; Link parsing + dispatch

(defconst gfm-present--heading-link-rx
  (rx bos "#" (group (+ (not (any "/" "?" "#" " ")))) eos)
  "Regexp matching `#<slug>'-form link URLs.")

(defconst gfm-present--any-heading-rx
  (rx bol (+ "#") " " (group (* nonl)) eol)
  "Regexp matching any markdown heading line.")

(defun gfm-present--parse-heading-link (url)
  "Return the slug from URL of form `#<slug>', or nil."
  (when (and (stringp url)
             (string-match gfm-present--heading-link-rx url))
    (match-string 1 url)))

(defun gfm-present--dispatch-heading-link (slug)
  "Locate the first heading whose slug equals SLUG.
On match, return (HEADING-POS . (REGION-START . REGION-END)) where
the region encloses the H1 containing the matched heading.  When
no match, return the symbol `pass-through'."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((found nil)
            (in-fence nil))
        (while (and (not found) (not (eobp)))
          (cond
           ((looking-at gfm-present--fence-rx)
            (setq in-fence (not in-fence)))
           ((and (not in-fence) (looking-at gfm-present--any-heading-rx))
            (let ((this-slug (gfm-present--heading-slug
                              (match-string-no-properties 1))))
              (when (equal this-slug slug)
                (setq found (line-beginning-position))))))
          (forward-line 1))
        (if found
            (cons found (gfm-present--heading-region found))
          'pass-through)))))

(defconst gfm-present--md-link-rx
  (rx "[" (group (* (not (any "[" "]" "\n")))) "]"
      "(" (group (* (not (any ")" "\n")))) ")")
  "Regexp matching a markdown `[label](url)' link.")

(defun gfm-present--link-url-at-point ()
  "Return the URL of the markdown link at point, or nil."
  (save-excursion
    (let ((p (point))
          (eol (line-end-position))
          (bol (line-beginning-position))
          found)
      (goto-char bol)
      (while (and (not found) (re-search-forward gfm-present--md-link-rx eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq found (match-string-no-properties 2))))
      found)))

(defun gfm-present--follow-link-fallback ()
  "Delegate link follow to `markdown-mode's default handler."
  (cond
   ((fboundp 'markdown-follow-link-at-point)
    (call-interactively #'markdown-follow-link-at-point))
   ((fboundp 'markdown-follow-thing-at-point)
    (call-interactively #'markdown-follow-thing-at-point))))

(defun gfm-present--follow-source-link (path start end)
  "Open PATH narrowed to lines START..END, with focus overlay."
  (let* ((resolved (if (file-name-absolute-p path) path
                     (expand-file-name path default-directory)))
         (buf (find-file-noselect resolved)))
    (pop-to-buffer buf)
    (gfm-present--render-narrowed-source buf start end start end)))

(defun gfm-present--follow-diff-link (parsed)
  "Open the magit diff range described by PARSED plist (§10)."
  (let ((base (plist-get parsed :base))
        (head (plist-get parsed :head))
        (path (plist-get parsed :path)))
    (unless (require 'magit nil t)
      (user-error "magit is required to follow diff links"))
    (when (fboundp 'magit-diff-range)
      (if path
          (magit-diff-range (format "%s...%s" base head) nil (list path))
        (magit-diff-range (format "%s...%s" base head))))))

(defun gfm-present-follow-link ()
  "Follow the markdown link at point.
Dispatches by URL form: heading slug, source-range, diff-range, or
pass-through to the markdown major mode default handler."
  (interactive)
  (let* ((url (gfm-present--link-url-at-point))
         (slug (and url (gfm-present--parse-heading-link url)))
         (source (and url (not slug)
                      (gfm-present--parse-source-link url)))
         (diff (and url (not slug) (not source)
                    (gfm-present--parse-diff-link url))))
    (cond
     (slug
      (let ((result (gfm-present--dispatch-heading-link slug)))
        (cond
         ((eq result 'pass-through)
          (gfm-present--follow-link-fallback))
         (t
          (push-mark (point) t)
          (let ((heading-pos (car result))
                (region (cdr result)))
            (widen)
            (narrow-to-region (car region) (cdr region))
            (goto-char heading-pos)
            (when gfm-present-mode
              (gfm-present--render-link-previews)))))))
     (source
      (push-mark (point) t)
      (gfm-present--follow-source-link
       (car source) (cadr source) (cddr source)))
     (diff
      ;; §10
      (push-mark (point) t)
      (gfm-present--follow-diff-link diff))
     (t
      (gfm-present--follow-link-fallback)))))


;;; Box-preview helpers

(defun gfm-present--abbrev-source-path (path)
  "Return PATH abbreviated for top-border display.
Project-relative when PATH is inside a project (resolved against
PATH's own directory, not the visiting buffer); otherwise
`abbreviate-file-name'.  Nil or empty PATH returns PATH unchanged."
  (cond
   ((or (null path) (and (stringp path) (string-empty-p path))) path)
   (t
    (let* ((abs (expand-file-name path))
           (dir (file-name-directory (or (file-truename abs) abs)))
           (proj (and dir
                      (let ((default-directory dir))
                        (project-current)))))
      (cond
       ((and proj (file-in-directory-p abs (project-root proj)))
        (file-relative-name abs (project-root proj)))
       (t (abbreviate-file-name abs)))))))

(defun gfm-present--fit-label-into-border (label budget)
  "Truncate LABEL to fit within BUDGET cells, preserving trailing range.
Returns LABEL when it already fits.  When LABEL contains a `/'
separator and the trailing path component (with the `:start-end'
range suffix) fits within BUDGET minus the leading `…/' marker,
returns `…/<basename>:<start>-<end>'.  Otherwise truncates with a
plain leading `…'."
  (cond
   ((or (null label) (<= (string-width label) (max 0 budget))) label)
   (t
    (let* ((slash (and (string-match-p "/" label)
                       (cl-position ?/ label :from-end t)))
           (tail (and slash (substring label (1+ slash))))
           (ellipsis-path (and tail (concat "…/" tail))))
      (cond
       ((and ellipsis-path (<= (string-width ellipsis-path) budget))
        ellipsis-path)
       (t
        (let* ((available (max 0 (1- budget)))
               (suffix (gfm-present--right-substring-by-width
                        label available)))
          (concat "…" suffix))))))))

(defun gfm-present--right-substring-by-width (s cells)
  "Return the trailing substring of S whose `string-width' is at most CELLS."
  (let ((len (length s))
        (i (length s))
        (acc 0))
    (while (and (> i 0)
                (let ((next (+ acc (char-width (aref s (1- i))))))
                  (when (<= next cells)
                    (setq acc next)
                    t)))
      (setq i (1- i)))
    (substring s i len)))

(defun gfm-present--truncate-line-to-width (line cell-budget)
  "Return LINE truncated to CELL-BUDGET cells with `…' on overflow.
Truncation is measured via `string-width' (cell count).  Text
properties on the retained head of LINE are preserved."
  (truncate-string-to-width line cell-budget nil nil "…"))

(defun gfm-present--standalone-link-p (link-start link-end)
  "Return non-nil when [LINK-START, LINK-END) span is a standalone link.
A link is standalone when its line, with the `[label](url)' span
removed, is whitespace plus at most one list-item marker (`- ',
`* ', `+ ', `<n>. ') or a blockquote marker (`> ').

Callers MUST pass explicit positions rather than rely on global
match-data, since intervening `string-match' calls in parser helpers
\(e.g. `gfm-present--parse-source-link') will have clobbered it."
  (save-excursion
    (goto-char link-start)
    (let ((bol (line-beginning-position)))
      (goto-char link-end)
      (let* ((eol (line-end-position))
             (rest (concat
                    (buffer-substring-no-properties bol link-start)
                    (buffer-substring-no-properties link-end eol))))
        (and (string-match-p
              (rx bos (* blank)
                  (? (or "- " "* " "+ " "> "
                         (: (+ digit) ". ")))
                  (* blank) eos)
              rest)
             t)))))

;;; Box renderer

(defun gfm-present--box-display (&rest plist)
  "Return a propertised multi-line box display string.
PLIST keys:
  :label       string for the top border (already abbreviated).
  :body        body string (multi-line, pre-fontified).
  :extra       integer; when > 0, embedded as `+N more lines' in
               the bottom border.
  :lhs-margin  non-nil for LHS-margin mode (`│' instead of `│ ',
               2-col decoration width to match `\\=`\\=`\\=`diff' fences).

Box width follows
`(min available-width (max 80 (+ longest-body-line decoration-w)))'
mirroring `gfm-pretty-fences--apply-bordered-display'."
  (let* ((label (plist-get plist :label))
         (body (or (plist-get plist :body) ""))
         (extra (or (plist-get plist :extra) 0))
         (lhs-margin (plist-get plist :lhs-margin))
         (deco-w (if lhs-margin 2 4))
         (avail (gfm-pretty--available-width))
         (body-lines (split-string body "\n"))
         (longest (apply #'max 0 (mapcar #'string-width body-lines)))
         (box-w (min avail (max 80 (+ longest deco-w))))
         (interior-w (max 1 (- box-w deco-w)))
         (face 'gfm-pretty-border-face)
         (fitted (gfm-present--fit-label-into-border
                  (or label "") (max 1 (- box-w 5))))
         (top (gfm-present--box-top-border fitted box-w face))
         (bot (gfm-present--box-bottom-border extra box-w face))
         (decorated (mapconcat
                     (lambda (line)
                       (gfm-present--box-line line interior-w
                                              lhs-margin face))
                     body-lines "\n")))
    (concat top "\n" decorated "\n" bot)))

(defun gfm-present--box-top-border (label width face)
  "Build the top-border string of WIDTH cells embedding LABEL.
LABEL and fill dashes carry FACE."
  (let* ((label-w (string-width label))
         ;; `┌─ ' (3) + LABEL + ` ' (1) + dashes + `┐' (1) = WIDTH.
         (fill-w (max 1 (- width 3 label-w 1 1))))
    (concat
     (propertize "┌─ " 'face face)
     (propertize label 'face face)
     (propertize " " 'face face)
     (propertize (make-string fill-w ?─) 'face face)
     (propertize "┐" 'face face))))

(defun gfm-present--box-bottom-border (extra width face)
  "Build the bottom-border string of WIDTH cells.
When EXTRA > 0 the border embeds `+N more lines'; otherwise it is a
bare run of `─' between corners.  FACE is applied throughout."
  (cond
   ((and (integerp extra) (> extra 0))
    (let* ((tag (format "+%d more lines" extra))
           (tag-w (string-width tag))
           (fill-w (max 1 (- width 3 tag-w 1 1))))
      (concat
       (propertize "└─ " 'face face)
       (propertize tag 'face face)
       (propertize " " 'face face)
       (propertize (make-string fill-w ?─) 'face face)
       (propertize "┘" 'face face))))
   (t
    (let ((fill-w (max 1 (- width 2))))
      (concat
       (propertize "└" 'face face)
       (propertize (make-string fill-w ?─) 'face face)
       (propertize "┘" 'face face))))))

(defun gfm-present--box-line (line interior-w lhs-margin face)
  "Return LINE padded/truncated to INTERIOR-W cells wrapped in box edges.
When LHS-MARGIN is non-nil the wrap is `│' / `│' with no inner
padding; otherwise `│ ' / ` │'.  Border glyphs carry FACE."
  (let* ((truncated (gfm-present--truncate-line-to-width line interior-w))
         (w (string-width truncated))
         (pad-w (max 0 (- interior-w w)))
         (padded (concat truncated (make-string pad-w ?\s))))
    (cond
     (lhs-margin
      (concat (propertize "│" 'face face)
              padded
              (propertize "│" 'face face)))
     (t
      (concat (propertize "│ " 'face face)
              padded
              (propertize " │" 'face face))))))

(defun gfm-present--abbrev-diff-refs (ref)
  "Return REF shortened to 7 chars when it is a 40-char hex SHA.
Branch names, tags and short refs are returned unchanged."
  (cond
   ((and (stringp ref)
         (string-match-p (rx bos (= 40 hex) eos) ref))
    (substring ref 0 7))
   (t ref)))

;;; Source-range link parsing + preview

(defconst gfm-present--source-link-rx
  (rx bos (group (+ (not (any "#"))))
      "#L" (group (+ digit))
      (? "-L" (group (+ digit)))
      eos)
  "Regexp matching a `path#L<start>[-L<end>]'-form link URL.")

(defun gfm-present--parse-source-link (url)
  "Return (PATH . (START . END)) for source-range URL, or nil."
  (when (and (stringp url)
             (string-match gfm-present--source-link-rx url))
    (let* ((path (match-string 1 url))
           (start (string-to-number (match-string 2 url)))
           (end-s (match-string 3 url))
           (end (if end-s (string-to-number end-s) start)))
      (cons path (cons start end)))))

(defun gfm-present--major-mode-for-path (path)
  "Return the major-mode symbol for PATH via `auto-mode-alist'.
Falls back to `fundamental-mode' when no entry matches."
  (or (assoc-default path auto-mode-alist #'string-match-p)
      #'fundamental-mode))

(defconst gfm-present--preview-cap 10
  "Maximum number of lines to include in a preview fence body.")

(defun gfm-present--read-line-range (path start end)
  "Read PATH lines START..END (1-based, inclusive).
Return (LINES . EXTRA) where LINES is up to 10 strings (no
newlines) and EXTRA is the count of additional lines past the cap.
Return symbol `file-not-found' or `invalid-range' on error."
  (cond
   ((not (and path (file-readable-p path))) 'file-not-found)
   ((or (not (integerp start)) (not (integerp end))
        (< start 1) (< end start))
    'invalid-range)
   (t
    (catch 'result
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (when (> (forward-line (1- start)) 0)
          (throw 'result 'invalid-range))
        (when (eobp)
          (throw 'result 'invalid-range))
        (let* ((requested (1+ (- end start)))
               (cap gfm-present--preview-cap)
               (take (min requested cap))
               (extra (max 0 (- requested cap)))
               (lines nil))
          (dotimes (_ take)
            (unless (eobp)
              (push (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))
                    lines)
              (forward-line 1)))
          (cons (nreverse lines) extra)))))))

(defun gfm-present--fontify-source (path lines)
  "Return LINES joined with newlines, fontified per PATH's major-mode.
Activates `gfm-present--major-mode-for-path' in a temp buffer with
`buffer-file-name' set to PATH (so tree-sitter modes that key off
the file name activate correctly), inserts the joined LINES, runs
`font-lock-ensure', and returns `buffer-substring' so the `face'
text properties survive into the caller."
  (with-temp-buffer
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (let ((mode (gfm-present--major-mode-for-path path)))
            (condition-case _err
                (funcall mode)
              (error (fundamental-mode))))
          (insert (mapconcat #'identity lines "\n"))
          (font-lock-ensure)
          (buffer-substring (point-min) (point-max)))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun gfm-present--source-preview-display (path start end)
  "Build the preview display string for source-range link PATH#L<start>-L<end>.
Returns a propertised string: either a `gfm-pretty-border-face' box
\(top border embeds `<abbrev-path>:<start>-<end>'; body is the
major-mode-fontified source lines; bottom border embeds `+N more
lines' when the requested range exceeded `gfm-present--preview-cap')
or a bare `shadow'-faced `[broken preview] …' sentinel on
file-not-found / invalid-range."
  (let* ((abbrev (gfm-present--abbrev-source-path path))
         (label (format "%s:%d-%d" (or abbrev path) start end))
         (result (gfm-present--read-line-range path start end)))
    (cond
     ((eq result 'file-not-found)
      (propertize (format "[broken preview] %s — file not found" label)
                  'face 'shadow))
     ((eq result 'invalid-range)
      (propertize (format "[broken preview] %s — invalid range" label)
                  'face 'shadow))
     (t
      (let* ((lines (car result))
             (extra (cdr result))
             (body (gfm-present--fontify-source path lines)))
        (gfm-present--box-display
         :label label :body body :extra extra :lhs-margin nil))))))

(defvar-local gfm-present--preview-overlays nil
  "List of preview overlays created in this buffer.
See `gfm-present--render-link-previews'.")

(defun gfm-present--clear-link-previews ()
  "Delete all preview overlays in the current buffer."
  (mapc #'delete-overlay gfm-present--preview-overlays)
  (setq gfm-present--preview-overlays nil))

(defun gfm-present--make-preview-overlay (link-start link-end fence)
  "Create a preview overlay covering LINK-START..LINK-END showing FENCE."
  (let ((ov (make-overlay link-start link-end)))
    (overlay-put ov 'display fence)
    (overlay-put ov 'gfm-present t)
    (push ov gfm-present--preview-overlays)
    ov))

;;; Diff-range link parsing + preview

(defun gfm-present--parse-diff-link (url)
  "Parse `diff:<base>...<head>[#<path>]'.
Returns (:base BASE :head HEAD :path PATH-OR-NIL) or nil."
  (when (and (stringp url) (string-prefix-p "diff:" url))
    (let* ((rest (substring url 5))
           (sep (string-match-p "\\.\\.\\." rest)))
      (when sep
        (let* ((base (substring rest 0 sep))
               (after (substring rest (+ sep 3)))
               (hash (string-match-p "#" after))
               (head (if hash (substring after 0 hash) after))
               (path (and hash (substring after (1+ hash)))))
          (when (and (> (length base) 0) (> (length head) 0))
            (list :base base :head head :path path)))))))

(defun gfm-present--diff-preview-argv (worktree base head &optional path)
  "Build argv for `git -C WORKTREE diff B...H [-- P]'."
  (let ((argv (list "git" "-C" worktree "diff"
                    (format "%s...%s" base head))))
    (if path
        (append argv (list "--" path))
      argv)))

(defun gfm-present--run-diff-preview (worktree base head &optional path)
  "Run `git diff B...H [-- P]' from WORKTREE.
Return a plist (:status STATUS :body BODY :extra EXTRA) where STATUS is
`ok', `no-changes', or `error'."
  (let* ((argv (gfm-present--diff-preview-argv worktree base head path))
         (cap gfm-present--preview-cap)
         (output nil)
         (exit nil))
    (with-temp-buffer
      (setq exit (apply #'call-process (car argv) nil t nil (cdr argv)))
      (setq output (buffer-string)))
    (cond
     ((not (zerop exit))
      (let ((first-line (car (split-string output "\n" t))))
        (list :status 'error :body (or first-line "git error") :extra 0)))
     ((zerop (length (string-trim output)))
      (list :status 'no-changes :body "(no changes)" :extra 0))
     (t
      (let* ((all-lines (split-string output "\n"))
             (lines (if (and all-lines
                             (string-empty-p (car (last all-lines))))
                        (butlast all-lines)
                      all-lines))
             (n (length lines))
             (take (min cap n))
             (head-lines (cl-subseq lines 0 take))
             (extra (max 0 (- n cap))))
        (list :status 'ok
              :body (mapconcat #'identity head-lines "\n")
              :extra extra))))))

(defun gfm-present--diff-preview-display (worktree base head path)
  "Build the preview display string for diff link BASE...HEAD[#PATH].
Returns a propertised string: either an LHS-margin
`gfm-pretty-border-face' box (top border embeds `<base>...<head>'
or `<base>...<head> — <path>', with 40-hex SHAs shortened to 7
chars; body is the first lines of `git diff' run in WORKTREE) or a
bare `shadow'-faced `[broken preview] …' sentinel on `no-changes' /
git-error states."
  (let* ((short-base (gfm-present--abbrev-diff-refs base))
         (short-head (gfm-present--abbrev-diff-refs head))
         (refs (format "%s...%s" short-base short-head))
         (label (if path (format "%s — %s" refs path) refs))
         (qualifier (if path (format "[%s]" path) ""))
         (sentinel-prefix (concat refs qualifier))
         (result (gfm-present--run-diff-preview worktree base head path))
         (status (plist-get result :status))
         (body (plist-get result :body))
         (extra (plist-get result :extra)))
    (cond
     ((eq status 'error)
      (propertize
       (format "[broken preview] %s — git error: %s"
               sentinel-prefix body)
       'face 'shadow))
     ((eq status 'no-changes)
      (propertize
       (format "[broken preview] %s — no changes" sentinel-prefix)
       'face 'shadow))
     (t
      (gfm-present--box-display
       :label label :body body :extra extra :lhs-margin t)))))

(defun gfm-present--render-link-previews ()
  "Scan the current narrowing and render preview overlays for standalone links.
Source-range links (`path#L<a>-L<b>') get a boxed source preview;
diff links (`diff:B...H[#P]') get a boxed diff preview.  Links that
do not satisfy `gfm-present--standalone-link-p' are left
undecorated."
  (gfm-present--clear-link-previews)
  (let ((wt default-directory))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward gfm-present--md-link-rx nil t)
        (let* ((url (match-string-no-properties 2))
               (link-start (match-beginning 0))
               (link-end (match-end 0))
               (source (gfm-present--parse-source-link url))
               (diff (and (not source) (gfm-present--parse-diff-link url))))
          (when (and (or source diff)
                     (gfm-present--standalone-link-p
                      link-start link-end))
            (cond
             (source
              (let* ((path (car source))
                     (start (cadr source))
                     (end (cddr source))
                     (resolved (if (file-name-absolute-p path) path
                                 (expand-file-name path default-directory)))
                     (display (gfm-present--source-preview-display
                               resolved start end)))
                (gfm-present--make-preview-overlay
                 link-start link-end display)))
             (diff
              (let ((display (gfm-present--diff-preview-display
                              wt
                              (plist-get diff :base)
                              (plist-get diff :head)
                              (plist-get diff :path))))
                (gfm-present--make-preview-overlay
                 link-start link-end display))))))))))


;;; Detached narrowed-source renderer (§13)

(defvar-local gfm-present--source-overlays nil
  "Buffer-local overlays applied by `gfm-present--render-narrowed-source'.")

(defvar-local gfm-present--source-restorer nil
  "Buffer-local thunk that reverts the state set up by the source renderer.")

(defun gfm-present--cleanup-source-render ()
  "Run the buffer-local restorer (if any) and clear source overlays."
  (mapc #'delete-overlay gfm-present--source-overlays)
  (setq gfm-present--source-overlays nil)
  (when (functionp gfm-present--source-restorer)
    (funcall gfm-present--source-restorer))
  (setq gfm-present--source-restorer nil)
  (remove-hook 'kill-buffer-hook #'gfm-present--cleanup-source-render t))

(defun gfm-present--render-narrowed-source (buffer start-line end-line
                                                     &optional focus-start focus-end)
  "Narrow BUFFER to lines START-LINE..END-LINE and apply focus overlays.
Sets BUFFER read-only and registers a `kill-buffer-hook' restorer.
When FOCUS-START..FOCUS-END is given, paints `gfm-present-focus-face'
one overlay per line from `point-at-bol' to `point-at-eol'."
  (with-current-buffer buffer
    (gfm-present--cleanup-source-render)
    (let ((prev-ro buffer-read-only)
          start-pos end-pos)
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- start-line))
          (setq start-pos (point))
          (goto-char (point-min))
          (forward-line end-line)
          (setq end-pos (point))))
      (widen)
      (narrow-to-region start-pos end-pos)
      (when (and focus-start focus-end)
        (save-excursion
          (cl-loop for ln from focus-start to focus-end
                   do (progn
                        (goto-char (point-min))
                        (forward-line (- ln start-line))
                        (let* ((bol (line-beginning-position))
                               (eol (line-end-position))
                               (ov (make-overlay bol eol)))
                          (overlay-put ov 'face 'gfm-present-focus-face)
                          (overlay-put ov 'gfm-present-source t)
                          (push ov gfm-present--source-overlays))))))
      (setq buffer-read-only t)
      (setq gfm-present--source-restorer
            (let ((ro prev-ro))
              (lambda ()
                (let ((inhibit-read-only t)) (widen))
                (setq buffer-read-only ro))))
      (add-hook 'kill-buffer-hook
                #'gfm-present--cleanup-source-render nil t)
      buffer)))

;;; Pretty-links anchor-jump subscription

(defun gfm-present--after-anchor-jump (target-pos)
  "Re-narrow to the H1 region containing TARGET-POS after a pretty-links jump.
Registered on `gfm-pretty-links-after-anchor-jump-functions' (buffer-
locally) while `gfm-present-mode' is on, so RET on a decorated anchor
link lands narrowed to the target's slide region.  Also refreshes link
preview overlays for the new slide."
  (gfm-present--narrow-to-heading-at target-pos)
  (gfm-present--render-link-previews))

(defun gfm-present--around-evil-jump (orig &rest args)
  "Around-advice for evil jump commands.
While `gfm-present-mode' is on, widens before delegating to ORIG so the
target jump-list position is reachable, then re-narrows to the slide
containing point and refreshes link previews.  Outside present-mode,
calls ORIG with ARGS unchanged."
  (if (bound-and-true-p gfm-present-mode)
      (progn (widen)
             (apply orig args)
             (gfm-present--narrow-to-heading-at (point))
             (gfm-present--render-link-previews))
    (apply orig args)))

(with-eval-after-load 'evil
  (advice-add 'evil-jump-backward :around #'gfm-present--around-evil-jump)
  (advice-add 'evil-jump-forward :around #'gfm-present--around-evil-jump))

(with-eval-after-load 'better-jumper
  (advice-add 'better-jumper-jump-backward :around
              #'gfm-present--around-evil-jump)
  (advice-add 'better-jumper-jump-forward :around
              #'gfm-present--around-evil-jump))

;;; Minor mode

(defvar-local gfm-present--owned-buffer nil
  "Non-nil when the current buffer was opened solely by `gfm-present-markdown'.")

(defvar-keymap gfm-present-mode-map
  :doc "Keymap for `gfm-present-mode'."
  "C-n"   #'gfm-present-next-slide
  "C-f"   #'gfm-present-next-slide
  "C-p"   #'gfm-present-previous-slide
  "C-b"   #'gfm-present-previous-slide
  "C-c q" #'gfm-present-quit
  "RET"   #'gfm-present-follow-link)

(with-eval-after-load 'evil
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map gfm-present-mode-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* '(normal motion visual) gfm-present-mode-map
                      (kbd "C-n")   #'gfm-present-next-slide
                      (kbd "C-f")   #'gfm-present-next-slide
                      (kbd "C-p")   #'gfm-present-previous-slide
                      (kbd "C-b")   #'gfm-present-previous-slide
                      (kbd "C-c q") #'gfm-present-quit
                      (kbd "RET")   #'gfm-present-follow-link)))

;;;###autoload
(define-minor-mode gfm-present-mode
  "Buffer-local presentation mode for a markdown document.
Enabling narrows to the H1 region containing point; disabling
widens.  The keymap is installed via `minor-mode-overriding-map-alist'
so it takes precedence over evil-state bindings."
  :lighter " Pres"
  :keymap gfm-present-mode-map
  (cond
   (gfm-present-mode
    (setq-local minor-mode-overriding-map-alist
                (cons (cons 'gfm-present-mode gfm-present-mode-map)
                      (assq-delete-all 'gfm-present-mode
                                       minor-mode-overriding-map-alist)))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (add-hook 'before-revert-hook #'gfm-present--remember-position nil t)
    (add-hook 'after-revert-hook #'gfm-present--restore-position nil t)
    (add-hook 'gfm-pretty-links-after-anchor-jump-functions
              #'gfm-present--after-anchor-jump nil t)
    (gfm-present--narrow-to-heading-at (point))
    (gfm-present--render-link-previews))
   (t
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'gfm-present-mode
                           minor-mode-overriding-map-alist))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (remove-hook 'before-revert-hook #'gfm-present--remember-position t)
    (remove-hook 'after-revert-hook #'gfm-present--restore-position t)
    (remove-hook 'gfm-pretty-links-after-anchor-jump-functions
                 #'gfm-present--after-anchor-jump t)
    (gfm-present--clear-link-previews)
    (widen))))

;;;###autoload
(defun gfm-present-markdown (file)
  "Open FILE and enable `gfm-present-mode' in the visiting buffer.
Signals a `user-error' when FILE is not a readable regular file."
  (interactive "fMarkdown file: ")
  (unless (and file (file-readable-p file) (file-regular-p file))
    (user-error "gfm-present-markdown: not a readable file: %s" file))
  (let ((existing (get-file-buffer (expand-file-name file))))
    (find-file file)
    (unless existing
      (setq-local gfm-present--owned-buffer t))
    (gfm-present-mode 1)))

(defun gfm-present-quit ()
  "End the presentation in the current buffer.
Disables `gfm-present-mode' (which widens) and buries the buffer.
Kills the buffer instead when it was opened solely by `gfm-present-markdown'."
  (interactive)
  (let ((owned gfm-present--owned-buffer)
        (buf (current-buffer)))
    (when gfm-present-mode (gfm-present-mode -1))
    (if owned
        (kill-buffer buf)
      (bury-buffer buf))))


;;; Revert resilience (§12)

(defvar-local gfm-present--revert-anchor nil
  "Plist captured before buffer revert.
Keys: :slug :index :fingerprint :window-start-offset.")
(put 'gfm-present--revert-anchor 'permanent-local t)

(defconst gfm-present--fingerprint-cap 80
  "Maximum number of characters captured for the revert fingerprint.")

(defun gfm-present--h1-text-at (pos)
  "Return the heading text of the H1 line beginning at POS, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (when (looking-at gfm-present--h1-rx)
        (match-string-no-properties 1)))))

(defun gfm-present--remember-position ()
  "Capture narrowing context into `gfm-present--revert-anchor'."
  (let* ((pt (point))
         (positions (save-restriction (widen)
                                      (gfm-present--all-h1-positions)))
         (current-h1 (gfm-present--current-h1-start))
         (slug-text (and current-h1 (gfm-present--h1-text-at current-h1)))
         (slug (and slug-text (gfm-present--heading-slug slug-text)))
         (index (and current-h1
                     (cl-position current-h1 positions :test #'=)))
         (fingerprint
          (save-restriction
            (widen)
            (let ((available (- (point-max) pt)))
              (when (> available 0)
                (buffer-substring-no-properties
                 pt (+ pt (min gfm-present--fingerprint-cap available)))))))
         (win (get-buffer-window (current-buffer)))
         (offset (if win (- (window-start win) pt) 0)))
    (setq gfm-present--revert-anchor
          (list :slug slug :index index
                :fingerprint fingerprint
                :window-start-offset offset))))

(defun gfm-present--find-h1-by-slug (slug)
  "Return the H1 position whose slug equals SLUG, or nil."
  (cl-loop for pos in (save-restriction (widen)
                                        (gfm-present--all-h1-positions))
           for text = (gfm-present--h1-text-at pos)
           for s = (and text (gfm-present--heading-slug text))
           when (equal s slug) return pos))

(defun gfm-present--restore-position ()
  "Re-narrow and restore point + scroll using `gfm-present--revert-anchor'.
When `kill-all-local-variables' has cleared `gfm-present-mode' during
the revert cycle, re-enable the mode so the buffer ends up in the same
state as before."
  (when gfm-present--revert-anchor
    (let* ((anchor gfm-present--revert-anchor)
           (slug (plist-get anchor :slug))
           (index (plist-get anchor :index))
           (fingerprint (plist-get anchor :fingerprint))
           (offset (or (plist-get anchor :window-start-offset) 0))
           (positions (save-restriction (widen)
                                        (gfm-present--all-h1-positions)))
           (target (or (and slug (gfm-present--find-h1-by-slug slug))
                       (and index positions
                            (nth (min index (1- (length positions)))
                                 positions))
                       (car positions))))
      (widen)
      (when target
        (goto-char target))
      (when (and fingerprint (> (length fingerprint) 0))
        (let ((from (or target (point-min))))
          (goto-char from)
          (when (search-forward fingerprint nil t)
            (goto-char (match-beginning 0)))))
      (setq gfm-present--revert-anchor nil)
      (unless gfm-present-mode
        (gfm-present-mode 1))
      (when gfm-present-mode
        (gfm-present--render-link-previews))
      (let ((win (get-buffer-window (current-buffer))))
        (when (and win fingerprint (> (length fingerprint) 0))
          (set-window-start
           win (max (point-min) (+ (point) offset))))))))

(put 'gfm-present--remember-position 'permanent-local-hook t)
(put 'gfm-present--restore-position 'permanent-local-hook t)

(provide 'gfm-present)
;;; gfm-present.el ends here
