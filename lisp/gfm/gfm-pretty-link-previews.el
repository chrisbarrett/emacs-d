;;; gfm-pretty-link-previews.el --- Source/diff link preview overlays for GFM -*- lexical-binding: t; -*-

;;; Commentary:

;; Decorator that renders box-bordered previews for standalone GFM
;; links pointing at source-range (`path#L<a>-L<b>') or diff-range
;; (`diff:<base>...<head>[#<path>]') destinations.  Originally lived
;; inside `gfm-present.el'; extracted so previews fire wherever
;; `gfm-pretty-mode' is enabled — not only inside a presentation.
;;
;; The decorator participates in the engine's standard lifecycle:
;; `:collect-fn' enumerates standalone matching links, `:apply-block-fn'
;; builds a per-window display overlay covering each link's
;; `[label](url)' span.  `:full-rebuild-required-p' returns t for any
;; edit since link detection is position-sensitive in ways that don't
;; reward scoped invalidation.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)

(defgroup gfm-pretty-link-previews nil
  "Source/diff link preview overlays for GFM markdown buffers."
  :group 'markdown-faces)

(defconst gfm-pretty-link-previews--preview-cap 10
  "Maximum number of lines to include in a preview body.")


;;; Overlay registry

(defconst gfm-pretty-link-previews--registry
  (gfm-pretty--registry-for 'link-previews 'gfm-pretty-link-previews)
  "Shared overlay-registry context for link-preview overlays.")


;;; Link regex + parsers

(defconst gfm-pretty-link-previews--md-link-rx
  (rx "[" (group (* (not (any "[" "]" "\n")))) "]"
      "(" (group (* (not (any ")" "\n")))) ")")
  "Regexp matching a markdown `[label](url)' link.")

(defconst gfm-pretty-link-previews--source-link-rx
  (rx bos (group (+ (not (any "#"))))
      "#L" (group (+ digit))
      (? "-L" (group (+ digit)))
      eos)
  "Regexp matching a `path#L<start>[-L<end>]'-form link URL.")

(defun gfm-pretty-link-previews--parse-source-link (url)
  "Return (PATH . (START . END)) for source-range URL, or nil."
  (when (and (stringp url)
             (string-match gfm-pretty-link-previews--source-link-rx url))
    (let* ((path (match-string 1 url))
           (start (string-to-number (match-string 2 url)))
           (end-s (match-string 3 url))
           (end (if end-s (string-to-number end-s) start)))
      (cons path (cons start end)))))

(defun gfm-pretty-link-previews--parse-diff-link (url)
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

(defun gfm-pretty-link-previews--standalone-link-p (link-start link-end)
  "Return non-nil when [LINK-START, LINK-END) span is a standalone link.
A link is standalone when its line, with the `[label](url)' span
removed, is whitespace plus at most one list-item marker (`- ',
`* ', `+ ', `<n>. ') or a blockquote marker (`> ').

Callers MUST pass explicit positions rather than rely on global
match-data, since intervening `string-match' calls in parser helpers
\(e.g. `gfm-pretty-link-previews--parse-source-link') will have
clobbered it."
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


;;; Path / refs abbreviation

(defun gfm-pretty-link-previews--abbrev-source-path (path)
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

(defun gfm-pretty-link-previews--abbrev-diff-refs (ref)
  "Return REF shortened to 7 chars when it is a 40-char hex SHA.
Branch names, tags and short refs are returned unchanged."
  (cond
   ((and (stringp ref)
         (string-match-p (rx bos (= 40 hex) eos) ref))
    (substring ref 0 7))
   (t ref)))


;;; Width helpers

(defun gfm-pretty-link-previews--right-substring-by-width (s cells)
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

(defun gfm-pretty-link-previews--fit-label-into-border (label budget)
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
               (suffix (gfm-pretty-link-previews--right-substring-by-width
                        label available)))
          (concat "…" suffix))))))))

(defun gfm-pretty-link-previews--truncate-line-to-width (line cell-budget)
  "Return LINE truncated to CELL-BUDGET cells with `…' on overflow.
Truncation is measured via `string-width' (cell count).  Text
properties on the retained head of LINE are preserved."
  (truncate-string-to-width line cell-budget nil nil "…"))


;;; Box renderer

(defun gfm-pretty-link-previews--box-top-border (label width face)
  "Build the top-border string of WIDTH cells embedding LABEL.
LABEL and fill dashes carry FACE."
  (let* ((label-w (string-width label))
         (fill-w (max 1 (- width 3 label-w 1 1))))
    (concat
     (propertize "┌─ " 'face face)
     (propertize label 'face face)
     (propertize " " 'face face)
     (propertize (make-string fill-w ?─) 'face face)
     (propertize "┐" 'face face))))

(defun gfm-pretty-link-previews--box-bottom-border (extra width face)
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

(defun gfm-pretty-link-previews--box-line (line interior-w lhs-margin face)
  "Return LINE padded/truncated to INTERIOR-W cells wrapped in box edges.
When LHS-MARGIN is non-nil the wrap is `│' / `│' with no inner
padding; otherwise `│ ' / ` │'.  Border glyphs carry FACE."
  (let* ((truncated (gfm-pretty-link-previews--truncate-line-to-width
                     line interior-w))
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

(defun gfm-pretty-link-previews--box-display (&rest plist)
  "Return a propertised multi-line box display string.
PLIST keys:
  :label       string for the top border (already abbreviated).
  :body        body string (multi-line, pre-fontified).
  :extra       integer; when > 0, embedded as `+N more lines' in
               the bottom border.
  :lhs-margin  non-nil for LHS-margin mode (`│' instead of `│ ',
               2-col decoration width to match `\\=`\\=`\\=`diff' fences).
  :window      target window for available-width measurement.

Box width follows
`(min available-width (max 80 (+ longest-body-line decoration-w)))'."
  (let* ((label (plist-get plist :label))
         (body (or (plist-get plist :body) ""))
         (extra (or (plist-get plist :extra) 0))
         (lhs-margin (plist-get plist :lhs-margin))
         (window (plist-get plist :window))
         (deco-w (if lhs-margin 2 4))
         (avail (gfm-pretty--available-width window))
         (body-lines (split-string body "\n"))
         (longest (apply #'max 0 (mapcar #'string-width body-lines)))
         (box-w (min avail (max 80 (+ longest deco-w))))
         (interior-w (max 1 (- box-w deco-w)))
         (face 'gfm-pretty-border-face)
         (fitted (gfm-pretty-link-previews--fit-label-into-border
                  (or label "") (max 1 (- box-w 5))))
         (top (gfm-pretty-link-previews--box-top-border fitted box-w face))
         (bot (gfm-pretty-link-previews--box-bottom-border extra box-w face))
         (decorated (mapconcat
                     (lambda (line)
                       (gfm-pretty-link-previews--box-line
                        line interior-w lhs-margin face))
                     body-lines "\n")))
    (concat top "\n" decorated "\n" bot)))


;;; Source-range preview

(defun gfm-pretty-link-previews--major-mode-for-path (path)
  "Return the major-mode symbol for PATH via `auto-mode-alist'.
Falls back to `fundamental-mode' when no entry matches."
  (or (assoc-default path auto-mode-alist #'string-match-p)
      #'fundamental-mode))

(defun gfm-pretty-link-previews--read-line-range (path start end)
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
               (cap gfm-pretty-link-previews--preview-cap)
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

(defun gfm-pretty-link-previews--fontify-source (path lines)
  "Return LINES joined with newlines, fontified per PATH's major-mode.
Activates the major-mode resolved for PATH in a temp buffer with
`buffer-file-name' set to PATH (so tree-sitter modes that key off
the file name activate correctly), inserts the joined LINES, runs
`font-lock-ensure', and returns `buffer-substring' so the `face'
text properties survive into the caller."
  (with-temp-buffer
    (setq buffer-file-name path)
    (unwind-protect
        (progn
          (let ((mode (gfm-pretty-link-previews--major-mode-for-path path)))
            (condition-case _err
                (funcall mode)
              (error (fundamental-mode))))
          (insert (mapconcat #'identity lines "\n"))
          (font-lock-ensure)
          (buffer-substring (point-min) (point-max)))
      (setq buffer-file-name nil)
      (set-buffer-modified-p nil))))

(defun gfm-pretty-link-previews--source-preview-display (path start end &optional window)
  "Build the preview display string for source-range link PATH#L<start>-L<end>.
WINDOW (optional) is forwarded to width measurement.

Returns a propertised string: either a `gfm-pretty-border-face' box
\(top border embeds `<abbrev-path>:<start>-<end>'; body is the
major-mode-fontified source lines; bottom border embeds `+N more
lines' when the requested range exceeded
`gfm-pretty-link-previews--preview-cap') or a bare `shadow'-faced
`[broken preview] …' sentinel on file-not-found / invalid-range."
  (let* ((abbrev (gfm-pretty-link-previews--abbrev-source-path path))
         (label (format "%s:%d-%d" (or abbrev path) start end))
         (result (gfm-pretty-link-previews--read-line-range path start end)))
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
             (body (gfm-pretty-link-previews--fontify-source path lines)))
        (gfm-pretty-link-previews--box-display
         :label label :body body :extra extra :lhs-margin nil
         :window window))))))


;;; Diff-range preview

(defun gfm-pretty-link-previews--diff-preview-argv (worktree base head &optional path)
  "Build argv for `git -C WORKTREE diff B...H [-- P]'."
  (let ((argv (list "git" "-C" worktree "diff"
                    (format "%s...%s" base head))))
    (if path
        (append argv (list "--" path))
      argv)))

(defun gfm-pretty-link-previews--run-diff-preview (worktree base head &optional path)
  "Run `git diff B...H [-- P]' from WORKTREE.
Return a plist (:status STATUS :body BODY :extra EXTRA) where STATUS is
`ok', `no-changes', or `error'."
  (let* ((argv (gfm-pretty-link-previews--diff-preview-argv
                worktree base head path))
         (cap gfm-pretty-link-previews--preview-cap)
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

(defun gfm-pretty-link-previews--diff-preview-display (worktree base head path &optional window)
  "Build the preview display string for diff link BASE...HEAD[#PATH].
WORKTREE is the directory `git diff' runs in.  WINDOW is forwarded
to width measurement.

Returns a propertised string: either an LHS-margin
`gfm-pretty-border-face' box (top border embeds `<base>...<head>'
or `<base>...<head> — <path>', with 40-hex SHAs shortened to 7
chars; body is the first lines of `git diff' run in WORKTREE) or a
bare `shadow'-faced `[broken preview] …' sentinel on `no-changes' /
git-error states."
  (let* ((short-base (gfm-pretty-link-previews--abbrev-diff-refs base))
         (short-head (gfm-pretty-link-previews--abbrev-diff-refs head))
         (refs (format "%s...%s" short-base short-head))
         (label (if path (format "%s — %s" refs path) refs))
         (qualifier (if path (format "[%s]" path) ""))
         (sentinel-prefix (concat refs qualifier))
         (result (gfm-pretty-link-previews--run-diff-preview
                  worktree base head path))
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
      (gfm-pretty-link-previews--box-display
       :label label :body body :extra extra :lhs-margin t
       :window window)))))


;;; Decorator protocol

(cl-defstruct (gfm-pretty-link-previews--block
               (:constructor gfm-pretty-link-previews--make-block)
               (:copier nil))
  "Tagged preview block for unified rebuild dispatch.
RANGE is (LINK-BEG . LINK-END) covering the `[label](url)' span.
PAYLOAD is (KIND . SPEC) where KIND is `source' or `diff':
  source: (PATH START END RESOLVED-ABS-PATH)
  diff:   (:base B :head H :path P :worktree W)"
  range payload)

(defun gfm-pretty-link-previews--collect-blocks ()
  "Scan the widened buffer for standalone source/diff links.
Returns a list of `gfm-pretty-link-previews--block' structs, one per
match.  Engine memoises via `gfm-pretty--collect'."
  (let ((wt default-directory)
        blocks)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward
                  gfm-pretty-link-previews--md-link-rx nil t)
            (let* ((url (match-string-no-properties 2))
                   (link-start (match-beginning 0))
                   (link-end (match-end 0))
                   (source (gfm-pretty-link-previews--parse-source-link url))
                   (diff (and (not source)
                              (gfm-pretty-link-previews--parse-diff-link
                               url))))
              (when (and (or source diff)
                         (gfm-pretty-link-previews--standalone-link-p
                          link-start link-end))
                (cond
                 (source
                  (let* ((path (car source))
                         (start (cadr source))
                         (end (cddr source))
                         (resolved (if (file-name-absolute-p path)
                                       path
                                     (expand-file-name
                                      path default-directory))))
                    (push (gfm-pretty-link-previews--make-block
                           :range (cons link-start link-end)
                           :payload (list 'source path start end resolved))
                          blocks)))
                 (diff
                  (push (gfm-pretty-link-previews--make-block
                         :range (cons link-start link-end)
                         :payload (list 'diff
                                        :base (plist-get diff :base)
                                        :head (plist-get diff :head)
                                        :path (plist-get diff :path)
                                        :worktree wt))
                        blocks)))))))))
    (nreverse blocks)))

(defun gfm-pretty-link-previews--apply-block (block window)
  "Apply a per-WINDOW display overlay for preview BLOCK."
  (save-restriction
    (widen)
    (let* ((range (gfm-pretty-link-previews--block-range block))
           (payload (gfm-pretty-link-previews--block-payload block))
           (kind (car payload))
           (display
            (cond
             ((eq kind 'source)
              ;; payload = (source PATH START END RESOLVED).
              (gfm-pretty-link-previews--source-preview-display
               (nth 4 payload)
               (nth 2 payload)
               (nth 3 payload)
               window))
             ((eq kind 'diff)
              (let ((spec (cdr payload)))
                (gfm-pretty-link-previews--diff-preview-display
                 (plist-get spec :worktree)
                 (plist-get spec :base)
                 (plist-get spec :head)
                 (plist-get spec :path)
                 window))))))
      (when display
        (gfm-pretty--make-display
         gfm-pretty-link-previews--registry
         (car range) (cdr range) window
         'gfm-pretty-link-previews-kind kind
         'evaporate t
         'display display)))))

(defun gfm-pretty-link-previews--full-rebuild-required-p (_dirty)
  "Always non-nil — any edit may reshape link detection."
  t)

(defun gfm-pretty-link-previews--rebuild ()
  "Tear down and re-apply preview overlays for the current buffer.
Delegates to the engine's generic rebuild path for the
`link-previews' decorator.  Useful from test contexts that need
to render previews without first toggling `gfm-pretty-mode'."
  (gfm-pretty--rebuild (gfm-pretty--get 'link-previews)))

(defun gfm-pretty-link-previews--on-enable ()
  "Per-decorator setup invoked on enable.  No-op."
  nil)

(defun gfm-pretty-link-previews--on-disable ()
  "Per-decorator teardown invoked on disable.  Engine handles overlays."
  nil)


;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'link-previews
    :registry           gfm-pretty-link-previews--registry
    :collect-fn         #'gfm-pretty-link-previews--collect-blocks
    :range-fn           #'gfm-pretty-link-previews--block-range
    :apply-block-fn     #'gfm-pretty-link-previews--apply-block
    :full-rebuild-required-p
                        #'gfm-pretty-link-previews--full-rebuild-required-p
    :on-enable-fn       #'gfm-pretty-link-previews--on-enable
    :on-disable-fn      #'gfm-pretty-link-previews--on-disable))

(provide 'gfm-pretty-link-previews)

;;; gfm-pretty-link-previews.el ends here
