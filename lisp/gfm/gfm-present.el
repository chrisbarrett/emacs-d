;;; gfm-present.el --- Slide walkthrough over markdown documents -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local minor-mode walk-through over a markdown document.  Top-
;; level H1 headings are slides; navigation narrows to the heading
;; region containing point.

;;; Code:

(require 'cl-lib)
(require 'gfm-pretty)

;; Forward declarations for byte-compiler.
(defvar gfm-present-mode nil)
(defvar gfm-present-mode-map)
(declare-function evil-define-key* "evil-core")
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
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

(defconst gfm-present--ext-to-lang
  '(("rs" . "rust") ("el" . "elisp") ("py" . "python")
    ("ts" . "typescript") ("tsx" . "typescript")
    ("js" . "javascript") ("jsx" . "javascript")
    ("go" . "go") ("md" . "markdown") ("c" . "c") ("h" . "c")
    ("cpp" . "cpp") ("hpp" . "cpp") ("cc" . "cpp")
    ("rb" . "ruby") ("java" . "java") ("kt" . "kotlin")
    ("swift" . "swift") ("hs" . "haskell")
    ("sh" . "bash") ("nix" . "nix"))
  "Alist of file extension (lowercase) to fence language name.")

(defun gfm-present--language-from-extension (path)
  "Return the fence language for PATH's extension, defaulting to \"text\"."
  (let ((ext (and path (file-name-extension path))))
    (or (and ext (cdr (assoc (downcase ext) gfm-present--ext-to-lang)))
        "text")))

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

(defun gfm-present--build-preview-fence (lang label path start end body extra)
  "Compose a fenced code-block string for a link preview.
Header is ``\\=`\\=`\\=`LANG LABEL · PATH:START-END\".  BODY is the
body (already a string with newline separators).  When EXTRA > 0,
append a footer line `+EXTRA more lines · click to open'."
  (let ((header (format "```%s %s · %s:%d-%d" lang label path start end)))
    (concat header "\n"
            body "\n"
            "```"
            (when (and extra (> extra 0))
              (format "\n+%d more lines · click to open" extra)))))

(defun gfm-present--source-preview-fence (path start end label)
  "Build the preview fence string for source-range link PATH#L<start>-L<end>."
  (let* ((lang (gfm-present--language-from-extension path))
         (result (gfm-present--read-line-range path start end)))
    (cond
     ((eq result 'file-not-found)
      (gfm-present--build-preview-fence
       "text" label path start end (format "(file not found: %s)" path) 0))
     ((eq result 'invalid-range)
      (gfm-present--build-preview-fence
       "text" label path start end "(invalid range)" 0))
     (t
      (let ((body (mapconcat #'identity (car result) "\n"))
            (extra (cdr result)))
        (gfm-present--build-preview-fence
         lang label path start end body extra))))))

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

(defun gfm-present--diff-preview-fence (worktree base head path label)
  "Build the preview fence string for diff link."
  (let* ((result (gfm-present--run-diff-preview worktree base head path))
         (status (plist-get result :status))
         (body (plist-get result :body))
         (extra (plist-get result :extra))
         (header (concat (format "```diff %s · %s...%s" label base head)
                         (if path (format " -- %s" path) "")))
         (body-text (if (eq status 'error)
                        (format "(git error: %s)" body)
                      body)))
    (concat header "\n"
            body-text "\n"
            "```"
            (when (and extra (> extra 0))
              (format "\n+%d more lines · click to open" extra)))))

(defun gfm-present--render-link-previews ()
  "Scan the current narrowing and render preview overlays for known link forms.
Source-range links (`path#L<a>-L<b>') get a fenced source preview;
diff links (`diff:B...H[#P]') get a fenced diff preview."
  (gfm-present--clear-link-previews)
  (let ((wt default-directory))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward gfm-present--md-link-rx nil t)
        (let* ((label (match-string-no-properties 1))
               (url (match-string-no-properties 2))
               (link-start (match-beginning 0))
               (link-end (match-end 0))
               (source (gfm-present--parse-source-link url))
               (diff (and (not source) (gfm-present--parse-diff-link url))))
          (cond
           (source
            (let* ((path (car source))
                   (start (cadr source))
                   (end (cddr source))
                   (resolved (if (file-name-absolute-p path) path
                               (expand-file-name path default-directory)))
                   (fence (gfm-present--source-preview-fence
                           resolved start end label)))
              (gfm-present--make-preview-overlay link-start link-end fence)))
           (diff
            (let ((fence (gfm-present--diff-preview-fence
                          wt
                          (plist-get diff :base)
                          (plist-get diff :head)
                          (plist-get diff :path)
                          label)))
              (gfm-present--make-preview-overlay
               link-start link-end fence)))))))))


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
    (gfm-present--narrow-to-heading-at (point))
    (gfm-present--render-link-previews))
   (t
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'gfm-present-mode
                           minor-mode-overriding-map-alist))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (remove-hook 'before-revert-hook #'gfm-present--remember-position t)
    (remove-hook 'after-revert-hook #'gfm-present--restore-position t)
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
