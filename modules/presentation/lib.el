;;; lib.el --- Presentation library -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local minor-mode walk-through over a markdown document.  Top-
;; level H1 headings are slides; navigation narrows to the heading
;; region containing point.

;;; Code:

(require 'cl-lib)

;; Forward declarations for byte-compiler.
(defvar +presentation-mode nil)
(defvar +presentation-mode-map)
(declare-function evil-define-key* "evil-core")
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function magit-diff-range "magit-diff")
(declare-function markdown-follow-link-at-point "markdown-mode")
(declare-function markdown-follow-thing-at-point "markdown-mode")

;;; Faces

(defface +presentation-focus-face
  '((((class color) (min-colors 89) (background dark))
     :background "grey20")
    (((class color) (min-colors 89) (background light))
     :background "grey90"))
  "Face for focus highlight in narrowed-source previews."
  :group '+presentation)

;;; Heading helpers

(defconst +presentation--fence-rx
  (rx bol (* blank) "```")
  "Regexp matching a markdown fenced-code-block delimiter line.")

(defconst +presentation--h1-rx
  (rx bol "# " (group (* nonl)) eol)
  "Regexp matching a top-level markdown heading line.")

(defun +presentation--all-h1-positions ()
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
           ((looking-at +presentation--fence-rx)
            (setq in-fence (not in-fence)))
           ((and (not in-fence) (looking-at +presentation--h1-rx))
            (push (line-beginning-position) positions)))
          (forward-line 1))
        (nreverse positions)))))

(defun +presentation--heading-region (pos)
  "Return (START . END) of the H1 region containing POS, or nil.
START is the beginning of the H1 line; END is either the beginning
of the next H1 line or `point-max'."
  (let* ((positions (+presentation--all-h1-positions))
         (preceding (cl-loop for p in positions while (<= p pos) collect p))
         (start (car (last preceding))))
    (when start
      (let* ((rest (cdr (member start positions)))
             (end (or (car rest) (point-max))))
        (cons start end)))))

(defun +presentation--heading-slug (text)
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

(defun +presentation--narrow-to-heading-at (pos)
  "Widen, then narrow to the H1 region containing POS.
When POS is before the first H1, narrow to the first H1's region.
When the document has no H1s, leave the buffer widened."
  (widen)
  (let ((region (+presentation--heading-region pos)))
    (cond
     (region
      (narrow-to-region (car region) (cdr region)))
     (t
      (let ((positions (+presentation--all-h1-positions)))
        (when positions
          (let* ((first (car positions))
                 (next (cadr positions))
                 (end (or next (point-max))))
            (narrow-to-region first end))))))))

;;; Navigation commands

(defun +presentation--current-h1-start ()
  "Return the buffer position of the H1 enclosing point, or nil."
  (save-restriction
    (widen)
    (car-safe (+presentation--heading-region (point)))))

(defun +presentation-next-slide ()
  "Advance to the next H1 and narrow there.  Silent no-op at last slide."
  (interactive)
  (let* ((positions (save-restriction
                      (widen)
                      (+presentation--all-h1-positions)))
         (current (+presentation--current-h1-start))
         (next (if current
                   (cadr (member current positions))
                 (car positions))))
    (when next
      (+presentation--narrow-to-heading-at next)
      (goto-char (point-min))
      (when +presentation-mode
        (+presentation--render-link-previews)))))

(defun +presentation-previous-slide ()
  "Retreat to the previous H1 and narrow there.  Silent no-op at first."
  (interactive)
  (let* ((positions (save-restriction
                      (widen)
                      (+presentation--all-h1-positions)))
         (current (+presentation--current-h1-start))
         (prev (when current
                 (cadr (member current (reverse positions))))))
    (when prev
      (+presentation--narrow-to-heading-at prev)
      (goto-char (point-min))
      (when +presentation-mode
        (+presentation--render-link-previews)))))

;;; Link parsing + dispatch

(defconst +presentation--heading-link-rx
  (rx bos "#" (group (+ (not (any "/" "?" "#" " ")))) eos)
  "Regexp matching `#<slug>'-form link URLs.")

(defconst +presentation--any-heading-rx
  (rx bol (+ "#") " " (group (* nonl)) eol)
  "Regexp matching any markdown heading line.")

(defun +presentation--parse-heading-link (url)
  "Return the slug from URL of form `#<slug>', or nil."
  (when (and (stringp url)
             (string-match +presentation--heading-link-rx url))
    (match-string 1 url)))

(defun +presentation--dispatch-heading-link (slug)
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
           ((looking-at +presentation--fence-rx)
            (setq in-fence (not in-fence)))
           ((and (not in-fence) (looking-at +presentation--any-heading-rx))
            (let ((this-slug (+presentation--heading-slug
                              (match-string-no-properties 1))))
              (when (equal this-slug slug)
                (setq found (line-beginning-position))))))
          (forward-line 1))
        (if found
            (cons found (+presentation--heading-region found))
          'pass-through)))))

(defconst +presentation--md-link-rx
  (rx "[" (group (* (not (any "[" "]" "\n")))) "]"
      "(" (group (* (not (any ")" "\n")))) ")")
  "Regexp matching a markdown `[label](url)' link.")

(defun +presentation--link-url-at-point ()
  "Return the URL of the markdown link at point, or nil."
  (save-excursion
    (let ((p (point))
          (eol (line-end-position))
          (bol (line-beginning-position))
          found)
      (goto-char bol)
      (while (and (not found) (re-search-forward +presentation--md-link-rx eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq found (match-string-no-properties 2))))
      found)))

(defun +presentation--follow-link-fallback ()
  "Delegate link follow to `markdown-mode's default handler."
  (cond
   ((fboundp 'markdown-follow-link-at-point)
    (call-interactively #'markdown-follow-link-at-point))
   ((fboundp 'markdown-follow-thing-at-point)
    (call-interactively #'markdown-follow-thing-at-point))))

(defun +presentation--follow-source-link (path start end)
  "Open PATH narrowed to lines START..END, with focus overlay."
  (let* ((resolved (if (file-name-absolute-p path) path
                     (expand-file-name path default-directory)))
         (buf (find-file-noselect resolved)))
    (pop-to-buffer buf)
    (+presentation--render-narrowed-source buf start end start end)))

(defun +presentation--follow-diff-link (parsed)
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

(defun +presentation-follow-link ()
  "Follow the markdown link at point.
Dispatches by URL form: heading slug, source-range, diff-range, or
pass-through to the markdown major mode default handler."
  (interactive)
  (let* ((url (+presentation--link-url-at-point))
         (slug (and url (+presentation--parse-heading-link url)))
         (source (and url (not slug)
                      (+presentation--parse-source-link url)))
         (diff (and url (not slug) (not source)
                    (+presentation--parse-diff-link url))))
    (cond
     (slug
      (let ((result (+presentation--dispatch-heading-link slug)))
        (cond
         ((eq result 'pass-through)
          (+presentation--follow-link-fallback))
         (t
          (push-mark (point) t)
          (let ((heading-pos (car result))
                (region (cdr result)))
            (widen)
            (narrow-to-region (car region) (cdr region))
            (goto-char heading-pos)
            (when +presentation-mode
              (+presentation--render-link-previews)))))))
     (source
      (push-mark (point) t)
      (+presentation--follow-source-link
       (car source) (cadr source) (cddr source)))
     (diff
      ;; §10
      (push-mark (point) t)
      (+presentation--follow-diff-link diff))
     (t
      (+presentation--follow-link-fallback)))))


;;; Source-range link parsing + preview

(defconst +presentation--source-link-rx
  (rx bos (group (+ (not (any "#"))))
      "#L" (group (+ digit))
      (? "-L" (group (+ digit)))
      eos)
  "Regexp matching a `path#L<start>[-L<end>]'-form link URL.")

(defun +presentation--parse-source-link (url)
  "Return (PATH . (START . END)) for source-range URL, or nil."
  (when (and (stringp url)
             (string-match +presentation--source-link-rx url))
    (let* ((path (match-string 1 url))
           (start (string-to-number (match-string 2 url)))
           (end-s (match-string 3 url))
           (end (if end-s (string-to-number end-s) start)))
      (cons path (cons start end)))))

(defconst +presentation--ext-to-lang
  '(("rs" . "rust") ("el" . "elisp") ("py" . "python")
    ("ts" . "typescript") ("tsx" . "typescript")
    ("js" . "javascript") ("jsx" . "javascript")
    ("go" . "go") ("md" . "markdown") ("c" . "c") ("h" . "c")
    ("cpp" . "cpp") ("hpp" . "cpp") ("cc" . "cpp")
    ("rb" . "ruby") ("java" . "java") ("kt" . "kotlin")
    ("swift" . "swift") ("hs" . "haskell")
    ("sh" . "bash") ("nix" . "nix"))
  "Alist of file extension (lowercase) to fence language name.")

(defun +presentation--language-from-extension (path)
  "Return the fence language for PATH's extension, defaulting to \"text\"."
  (let ((ext (and path (file-name-extension path))))
    (or (and ext (cdr (assoc (downcase ext) +presentation--ext-to-lang)))
        "text")))

(defconst +presentation--preview-cap 10
  "Maximum number of lines to include in a preview fence body.")

(defun +presentation--read-line-range (path start end)
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
               (cap +presentation--preview-cap)
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

(defun +presentation--build-preview-fence (lang label path start end body extra)
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

(defun +presentation--source-preview-fence (path start end label)
  "Build the preview fence string for source-range link PATH#L<start>-L<end>."
  (let* ((lang (+presentation--language-from-extension path))
         (result (+presentation--read-line-range path start end)))
    (cond
     ((eq result 'file-not-found)
      (+presentation--build-preview-fence
       "text" label path start end (format "(file not found: %s)" path) 0))
     ((eq result 'invalid-range)
      (+presentation--build-preview-fence
       "text" label path start end "(invalid range)" 0))
     (t
      (let ((body (mapconcat #'identity (car result) "\n"))
            (extra (cdr result)))
        (+presentation--build-preview-fence
         lang label path start end body extra))))))

(defvar-local +presentation--preview-overlays nil
  "List of preview overlays created in this buffer.
See `+presentation--render-link-previews'.")

(defun +presentation--clear-link-previews ()
  "Delete all preview overlays in the current buffer."
  (mapc #'delete-overlay +presentation--preview-overlays)
  (setq +presentation--preview-overlays nil))

(defun +presentation--make-preview-overlay (link-start link-end fence)
  "Create a preview overlay covering LINK-START..LINK-END showing FENCE."
  (let ((ov (make-overlay link-start link-end)))
    (overlay-put ov 'display fence)
    (overlay-put ov '+presentation t)
    (push ov +presentation--preview-overlays)
    ov))

;;; Diff-range link parsing + preview

(defun +presentation--parse-diff-link (url)
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

(defun +presentation--diff-preview-argv (worktree base head &optional path)
  "Build argv for `git -C WORKTREE diff B...H [-- P]'."
  (let ((argv (list "git" "-C" worktree "diff"
                    (format "%s...%s" base head))))
    (if path
        (append argv (list "--" path))
      argv)))

(defun +presentation--run-diff-preview (worktree base head &optional path)
  "Run `git diff B...H [-- P]' from WORKTREE.
Return a plist (:status STATUS :body BODY :extra EXTRA) where STATUS is
`ok', `no-changes', or `error'."
  (let* ((argv (+presentation--diff-preview-argv worktree base head path))
         (cap +presentation--preview-cap)
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

(defun +presentation--diff-preview-fence (worktree base head path label)
  "Build the preview fence string for diff link."
  (let* ((result (+presentation--run-diff-preview worktree base head path))
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

(defun +presentation--render-link-previews ()
  "Scan the current narrowing and render preview overlays for known link forms.
Source-range links (`path#L<a>-L<b>') get a fenced source preview;
diff links (`diff:B...H[#P]') get a fenced diff preview."
  (+presentation--clear-link-previews)
  (let ((wt default-directory))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward +presentation--md-link-rx nil t)
        (let* ((label (match-string-no-properties 1))
               (url (match-string-no-properties 2))
               (link-start (match-beginning 0))
               (link-end (match-end 0))
               (source (+presentation--parse-source-link url))
               (diff (and (not source) (+presentation--parse-diff-link url))))
          (cond
           (source
            (let* ((path (car source))
                   (start (cadr source))
                   (end (cddr source))
                   (resolved (if (file-name-absolute-p path) path
                               (expand-file-name path default-directory)))
                   (fence (+presentation--source-preview-fence
                           resolved start end label)))
              (+presentation--make-preview-overlay link-start link-end fence)))
           (diff
            (let ((fence (+presentation--diff-preview-fence
                          wt
                          (plist-get diff :base)
                          (plist-get diff :head)
                          (plist-get diff :path)
                          label)))
              (+presentation--make-preview-overlay
               link-start link-end fence)))))))))


;;; Detached narrowed-source renderer (§13)

(defvar-local +presentation--source-overlays nil
  "Buffer-local overlays applied by `+presentation--render-narrowed-source'.")

(defvar-local +presentation--source-restorer nil
  "Buffer-local thunk that reverts the state set up by the source renderer.")

(defun +presentation--cleanup-source-render ()
  "Run the buffer-local restorer (if any) and clear source overlays."
  (mapc #'delete-overlay +presentation--source-overlays)
  (setq +presentation--source-overlays nil)
  (when (functionp +presentation--source-restorer)
    (funcall +presentation--source-restorer))
  (setq +presentation--source-restorer nil)
  (remove-hook 'kill-buffer-hook #'+presentation--cleanup-source-render t))

(defun +presentation--render-narrowed-source (buffer start-line end-line
                                                     &optional focus-start focus-end)
  "Narrow BUFFER to lines START-LINE..END-LINE and apply focus overlays.
Sets BUFFER read-only and registers a `kill-buffer-hook' restorer.
When FOCUS-START..FOCUS-END is given, paints `+presentation-focus-face'
one overlay per line from `point-at-bol' to `point-at-eol'."
  (with-current-buffer buffer
    (+presentation--cleanup-source-render)
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
                          (overlay-put ov 'face '+presentation-focus-face)
                          (overlay-put ov '+presentation-source t)
                          (push ov +presentation--source-overlays))))))
      (setq buffer-read-only t)
      (setq +presentation--source-restorer
            (let ((ro prev-ro))
              (lambda ()
                (let ((inhibit-read-only t)) (widen))
                (setq buffer-read-only ro))))
      (add-hook 'kill-buffer-hook
                #'+presentation--cleanup-source-render nil t)
      buffer)))

;;; Minor mode

(defvar-local +presentation--owned-buffer nil
  "Non-nil when the current buffer was opened solely by `+present-markdown'.")

(defvar-keymap +presentation-mode-map
  :doc "Keymap for `+presentation-mode'."
  "C-n"   #'+presentation-next-slide
  "C-f"   #'+presentation-next-slide
  "C-p"   #'+presentation-previous-slide
  "C-b"   #'+presentation-previous-slide
  "C-c q" #'+presentation-quit
  "RET"   #'+presentation-follow-link)

(with-eval-after-load 'evil
  (when (fboundp 'evil-make-overriding-map)
    (evil-make-overriding-map +presentation-mode-map 'normal))
  (when (fboundp 'evil-define-key*)
    (evil-define-key* '(normal motion visual) +presentation-mode-map
                      (kbd "C-n")   #'+presentation-next-slide
                      (kbd "C-f")   #'+presentation-next-slide
                      (kbd "C-p")   #'+presentation-previous-slide
                      (kbd "C-b")   #'+presentation-previous-slide
                      (kbd "C-c q") #'+presentation-quit
                      (kbd "RET")   #'+presentation-follow-link)))

(define-minor-mode +presentation-mode
  "Buffer-local presentation mode for a markdown document.
Enabling narrows to the H1 region containing point; disabling
widens.  The keymap is installed via `minor-mode-overriding-map-alist'
so it takes precedence over evil-state bindings."
  :lighter " Pres"
  :keymap +presentation-mode-map
  (cond
   (+presentation-mode
    (setq-local minor-mode-overriding-map-alist
                (cons (cons '+presentation-mode +presentation-mode-map)
                      (assq-delete-all '+presentation-mode
                                       minor-mode-overriding-map-alist)))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (add-hook 'before-revert-hook #'+presentation--remember-position nil t)
    (add-hook 'after-revert-hook #'+presentation--restore-position nil t)
    (+presentation--narrow-to-heading-at (point))
    (+presentation--render-link-previews))
   (t
    (setq minor-mode-overriding-map-alist
          (assq-delete-all '+presentation-mode
                           minor-mode-overriding-map-alist))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (remove-hook 'before-revert-hook #'+presentation--remember-position t)
    (remove-hook 'after-revert-hook #'+presentation--restore-position t)
    (+presentation--clear-link-previews)
    (widen))))

;;;###autoload
(defun +present-markdown (file)
  "Open FILE and enable `+presentation-mode' in the visiting buffer.
Signals a `user-error' when FILE is not a readable regular file."
  (interactive "fMarkdown file: ")
  (unless (and file (file-readable-p file) (file-regular-p file))
    (user-error "+present-markdown: not a readable file: %s" file))
  (let ((existing (get-file-buffer (expand-file-name file))))
    (find-file file)
    (unless existing
      (setq-local +presentation--owned-buffer t))
    (+presentation-mode 1)))

(defun +presentation-quit ()
  "End the presentation in the current buffer.
Disables `+presentation-mode' (which widens) and buries the buffer.
Kills the buffer instead when it was opened solely by `+present-markdown'."
  (interactive)
  (let ((owned +presentation--owned-buffer)
        (buf (current-buffer)))
    (when +presentation-mode (+presentation-mode -1))
    (if owned
        (kill-buffer buf)
      (bury-buffer buf))))


;;; Revert resilience (§12)

(defvar-local +presentation--revert-anchor nil
  "Plist captured before buffer revert.
Keys: :slug :index :fingerprint :window-start-offset.")

(defconst +presentation--fingerprint-cap 80
  "Maximum number of characters captured for the revert fingerprint.")

(defun +presentation--h1-text-at (pos)
  "Return the heading text of the H1 line beginning at POS, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char pos)
      (when (looking-at +presentation--h1-rx)
        (match-string-no-properties 1)))))

(defun +presentation--remember-position ()
  "Capture narrowing context into `+presentation--revert-anchor'."
  (let* ((pt (point))
         (positions (save-restriction (widen)
                                      (+presentation--all-h1-positions)))
         (current-h1 (+presentation--current-h1-start))
         (slug-text (and current-h1 (+presentation--h1-text-at current-h1)))
         (slug (and slug-text (+presentation--heading-slug slug-text)))
         (index (and current-h1
                     (cl-position current-h1 positions :test #'=)))
         (fingerprint
          (save-restriction
            (widen)
            (let ((available (- (point-max) pt)))
              (when (> available 0)
                (buffer-substring-no-properties
                 pt (+ pt (min +presentation--fingerprint-cap available)))))))
         (win (get-buffer-window (current-buffer)))
         (offset (if win (- (window-start win) pt) 0)))
    (setq +presentation--revert-anchor
          (list :slug slug :index index
                :fingerprint fingerprint
                :window-start-offset offset))))

(defun +presentation--find-h1-by-slug (slug)
  "Return the H1 position whose slug equals SLUG, or nil."
  (cl-loop for pos in (save-restriction (widen)
                                        (+presentation--all-h1-positions))
           for text = (+presentation--h1-text-at pos)
           for s = (and text (+presentation--heading-slug text))
           when (equal s slug) return pos))

(defun +presentation--restore-position ()
  "Re-narrow and restore point + scroll using `+presentation--revert-anchor'."
  (when +presentation--revert-anchor
    (let* ((anchor +presentation--revert-anchor)
           (slug (plist-get anchor :slug))
           (index (plist-get anchor :index))
           (fingerprint (plist-get anchor :fingerprint))
           (offset (or (plist-get anchor :window-start-offset) 0))
           (positions (save-restriction (widen)
                                        (+presentation--all-h1-positions)))
           (target (or (and slug (+presentation--find-h1-by-slug slug))
                       (and index positions
                            (nth (min index (1- (length positions)))
                                 positions))
                       (car positions))))
      (widen)
      (when target
        (+presentation--narrow-to-heading-at target)
        (goto-char (point-min)))
      (when (and fingerprint (> (length fingerprint) 0))
        (goto-char (point-min))
        (when (search-forward fingerprint nil t)
          (goto-char (match-beginning 0))
          (let ((win (get-buffer-window (current-buffer))))
            (when win
              (set-window-start
               win (max (point-min) (+ (point) offset)))))))
      (setq +presentation--revert-anchor nil)
      (when +presentation-mode
        (+presentation--render-link-previews)))))

;;; lib.el ends here
