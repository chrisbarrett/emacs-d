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
(require 'gfm-pretty-engine)
(require 'gfm-pretty-link-previews)

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
(declare-function pulsar-highlight-pulse "pulsar" (&optional locus))

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

;;; Header-line counter

(defun gfm-present--refresh-header ()
  "Recompute the slide-count `header-line-format' buffer-locally.
Sets the value to a string like \"2/5\" when point is inside an H1's
region, or nil when the document has no H1s (or point precedes the
first H1)."
  (let* ((positions (gfm-present--all-h1-positions))
         (current (gfm-present--current-h1-start))
         (index (and current
                     (cl-position current positions :test #'=))))
    (setq header-line-format
          (and index (format "%d/%d" (1+ index) (length positions))))))

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
      (gfm-present--refresh-header)
      (when gfm-present-mode
        (gfm-present--rebuild-link-previews)))))

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
      (gfm-present--refresh-header)
      (when gfm-present-mode
        (gfm-present--rebuild-link-previews)))))

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

(defun gfm-present--link-url-at-point ()
  "Return the URL of the markdown link at point, or nil."
  (save-excursion
    (let ((p (point))
          (eol (line-end-position))
          (bol (line-beginning-position))
          found)
      (goto-char bol)
      (while (and (not found)
                  (re-search-forward
                   gfm-pretty-link-previews--md-link-rx eol t))
        (when (and (<= (match-beginning 0) p) (<= p (match-end 0)))
          (setq found (match-string-no-properties 2))))
      found)))

(defun gfm-present--rebuild-link-previews ()
  "Trigger a link-previews decorator rebuild when it is enabled."
  (when (gfm-pretty--state-get 'link-previews 'enabled-p)
    (gfm-pretty--rebuild (gfm-pretty--get 'link-previews))))

(defun gfm-present--follow-link-fallback ()
  "Delegate link follow to `markdown-mode's default handler."
  (cond
   ((fboundp 'markdown-follow-link-at-point)
    (call-interactively #'markdown-follow-link-at-point))
   ((fboundp 'markdown-follow-thing-at-point)
    (call-interactively #'markdown-follow-thing-at-point))))

(defun gfm-present--follow-source-link (path start end)
  "Open PATH at line START, pulsing lines START..END inclusive."
  (gfm-pretty-link-previews--follow-source-link path start end))

(defun gfm-present--follow-diff-link (parsed)
  "Open the diff range described by PARSED plist."
  (gfm-pretty-link-previews--follow-diff-link parsed default-directory))

(defun gfm-present-follow-link ()
  "Follow the markdown link at point.
Dispatches by URL form: heading slug, source-range, diff-range, or
pass-through to the markdown major mode default handler."
  (interactive)
  (let* ((url (gfm-present--link-url-at-point))
         (slug (and url (gfm-present--parse-heading-link url)))
         (source (and url (not slug)
                      (gfm-pretty-link-previews--parse-source-link url)))
         (diff (and url (not slug) (not source)
                    (gfm-pretty-link-previews--parse-diff-link url))))
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
            (gfm-present--refresh-header)
            (when gfm-present-mode
              (gfm-present--rebuild-link-previews)))))))
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





;;; Pretty-links anchor-jump subscription

(defun gfm-present--after-anchor-jump (target-pos)
  "Re-narrow to the H1 region containing TARGET-POS after a pretty-links jump.
Registered on `gfm-pretty-links-after-anchor-jump-functions' (buffer-
locally) while `gfm-present-mode' is on, so RET on a decorated anchor
link lands narrowed to the target's slide region.  Also refreshes link
preview overlays for the new slide."
  (gfm-present--narrow-to-heading-at target-pos)
  (gfm-present--refresh-header)
  (gfm-present--rebuild-link-previews))

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
             (gfm-present--rebuild-link-previews))
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

(defun gfm-present--exit ()
  "Disable `gfm-present-mode' in the current buffer.
Bound via `[remap widen]' so `C-x n w' ends the presentation cleanly
instead of leaving the keymap active over a widened buffer; the
mode-disable branch already widens."
  (interactive)
  (gfm-present-mode -1))

(defvar-keymap gfm-present-mode-map
  :doc "Keymap for `gfm-present-mode'."
  "C-n"          #'gfm-present-next-slide
  "C-f"          #'gfm-present-next-slide
  "C-p"          #'gfm-present-previous-slide
  "C-b"          #'gfm-present-previous-slide
  "C-c q"        #'gfm-present-quit
  "RET"          #'gfm-present-follow-link
  "<remap> <widen>" #'gfm-present--exit)

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
    (gfm-present--refresh-header)
    (unless (bound-and-true-p gfm-pretty-mode)
      (gfm-pretty-mode 1))
    (gfm-present--rebuild-link-previews))
   (t
    (setq minor-mode-overriding-map-alist
          (assq-delete-all 'gfm-present-mode
                           minor-mode-overriding-map-alist))
    (when (fboundp 'evil-normalize-keymaps) (evil-normalize-keymaps))
    (remove-hook 'before-revert-hook #'gfm-present--remember-position t)
    (remove-hook 'after-revert-hook #'gfm-present--restore-position t)
    (remove-hook 'gfm-pretty-links-after-anchor-jump-functions
                 #'gfm-present--after-anchor-jump t)
    (setq header-line-format nil)
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
      (gfm-present--refresh-header)
      (when gfm-present-mode
        (gfm-present--rebuild-link-previews))
      (let ((win (get-buffer-window (current-buffer))))
        (when (and win fingerprint (> (length fingerprint) 0))
          (set-window-start
           win (max (point-min) (+ (point) offset))))))))

(put 'gfm-present--remember-position 'permanent-local-hook t)
(put 'gfm-present--restore-position 'permanent-local-hook t)

(provide 'gfm-present)
;;; gfm-present.el ends here
