;;; +code-fences.el --- Polymode code fence decorations -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual decorations and compatibility fixes for polymode inner spans.
;; Box-drawing overlays, active-state dimming, separedit integration,
;; and guards for font-lock/syntax in indirect buffers.

;;; Code:

(defface +polymode-fence-face
  '((t :inherit font-lock-keyword-face :slant italic :weight normal :underline nil :overline nil))
  "Face for polymode head/tail fence lines.")

(defface +polymode-interpolation-face
  '((t :inherit warning :weight bold))
  "Face for interpolation expressions in unquoted polymode bodies.")


(defvar +code-fences-host-config nil
  "Alist of (HOST-MODE . PLIST).
Each entry maps a polymode host major-mode symbol to a plist with
optional keys :head-valid-p, :unquoted-p, and :interpolation-fn.")

(defun +code-fences-register (host-mode &rest plist)
  "Register HOST-MODE with code-fences dispatch callbacks.
PLIST may contain :head-valid-p, :unquoted-p, and :interpolation-fn."
  (setf (alist-get host-mode +code-fences-host-config) plist))

(defvar-local +code-fences--cached-config nil
  "Cached host config plist for this buffer, set during span decoration.")
(put '+code-fences--cached-config 'permanent-local t)

(defun +code-fences--config ()
  "Return the host config plist for the current polymode host, or nil.
Uses a buffer-local cache set during `+polymode-refontify-inner-spans'.
Falls back to resolving via `pm/polymode' if the cache is empty."
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (or (buffer-local-value '+code-fences--cached-config base)
        (when (bound-and-true-p pm/polymode)
          (let* ((hostmode-obj (eieio-oref pm/polymode '-hostmode))
                 (host-mode (and hostmode-obj (eieio-oref hostmode-obj 'mode)))
                 (config (and host-mode (alist-get host-mode +code-fences-host-config))))
            (when config
              (with-current-buffer base
                (setq +code-fences--cached-config config)))
            config)))))

;;; Separedit compatibility — detect polymode body spans as edit blocks.

(with-eval-after-load 'separedit
  (define-advice separedit--block-info (:before-until () polymode-span)
    "In polymode body span, return block info from span boundaries."
    (when (bound-and-true-p pm/chunkmode)
      (let* ((span (pm-innermost-span))
             (type (nth 0 span))
             (beg (nth 1 span))
             (end (nth 2 span)))
        (when (eq type 'body)
          (let* ((real-beg (save-excursion
                             (goto-char beg)
                             (skip-chars-forward " \t\n" end)
                             (line-beginning-position)))
                 (real-end (save-excursion
                             (goto-char end)
                             (skip-chars-backward " \t\n" real-beg)
                             (when (not (eolp)) (end-of-line))
                             (point)))
                 (indent (save-excursion
                           (goto-char real-beg)
                           (skip-chars-forward " \t")
                           (current-column))))
            (list :beginning real-beg
                  :end real-end
                  :lang-mode major-mode
                  :string-quotes ""
                  :indent-length indent)))))))

(define-advice poly-lock-adjust-span-face (:around (fn span) nil-bg-guard)
  "Skip span face adjustment when default background is unusable."
  (let ((bg (face-background 'default)))
    (when (and (stringp bg) (color-defined-p bg))
      (funcall fn span))))

(define-advice sh-syntax-propertize-function (:before-until (&rest _) polymode-guard)
  "Prevent sh-syntax-propertize from running without required sh-mode state.
bash-ts-mode derives from sh-mode but doesn't initialise sh-mode locals."
  (not (stringp (bound-and-true-p sh--heredoc-subsequent-re))))

(define-advice flymake-start (:around (fn &rest args) polymode-narrow)
  "Narrow polymode inner buffers to their span before flymake checks.
Filters out checkdoc — fragments lack file structure it expects."
  (if (buffer-base-buffer)
      (when-let* ((span (ignore-errors (pm-innermost-span)))
                  (_ (eq (nth 0 span) 'body)))
        (save-restriction
          (narrow-to-region (nth 1 span) (nth 2 span))
          (let ((flymake-diagnostic-functions
                 (remq 'elisp-flymake-checkdoc flymake-diagnostic-functions)))
            (apply fn args))))
    (apply fn args)))

(defvar-local +polymode--spans-decorated nil
  "Non-nil when polymode spans have been decorated.")

(defface +polymode-fence-dim-face
  '((t :foreground "gray30"))
  "Face for box-drawing characters on inactive polymode fences.
Foreground is recomputed at runtime via `+polymode--ensure-dim-face'.")

(defvar +polymode--fence-dim-hex nil
  "Cached hex color for `+polymode-fence-dim-face' foreground.")

(defvar +polymode--fence-dim-last-fg nil
  "Default face :foreground when fence dim face was last computed.")

(defvar +polymode--fence-dim-last-bg nil
  "Default face :background when fence dim face was last computed.")

(defun +polymode--resolve-default-color (attr fallback)
  "Resolve default face ATTR, trying GUI frames when current is unspecified.
FALLBACK used when no frame provides a valid color."
  (cl-loop for frame in (cons nil (frame-list))
           for val = (face-attribute 'default attr frame t)
           when (and (stringp val) (color-name-to-rgb val))
           return val
           finally return fallback))

(defun +polymode--ensure-dim-face ()
  "Set `+polymode-fence-dim-face' foreground to 20% default fg + 80% bg.
Cached; recomputed only when default face colors change."
  (let* ((fg (+polymode--resolve-default-color :foreground "white"))
         (bg (+polymode--resolve-default-color :background "black")))
    (unless (and +polymode--fence-dim-hex
                 (equal fg +polymode--fence-dim-last-fg)
                 (equal bg +polymode--fence-dim-last-bg))
      (let* ((fg-rgb (color-name-to-rgb fg))
             (bg-rgb (color-name-to-rgb bg))
             (hex (apply #'format "#%02x%02x%02x"
                         (mapcar (lambda (i)
                                   (round (* 255 (+ (* 0.2 (nth i fg-rgb))
                                                    (* 0.8 (nth i bg-rgb))))))
                                 '(0 1 2)))))
        (setq +polymode--fence-dim-hex hex
              +polymode--fence-dim-last-fg fg
              +polymode--fence-dim-last-bg bg)
        (set-face-attribute '+polymode-fence-dim-face nil :foreground hex)))))

(defun +polymode--rebuild-head-connector (ov &optional active)
  "Rebuild the connector in head overlay OV's after-string.
When ACTIVE, use the bright header line; otherwise use the dim one."
  (let* ((base (overlay-buffer ov))
         (fbeg (overlay-start ov))
         (col-c (overlay-get ov '+polymode-head-col-c))
         (base-line (overlay-get ov (if active
                                        '+polymode-head-base-line
                                      '+polymode-head-dim-line)))
         (cface (if active 'font-lock-comment-delimiter-face '+polymode-fence-dim-face))
         (col-h (with-current-buffer base
                  (save-excursion
                    (goto-char fbeg)
                    (end-of-line)
                    (current-column))))
         (connector (when (and col-h (>= col-h col-c))
                      (concat (propertize (make-string (- col-h col-c) ?─) 'face cface)
                              (propertize "╯" 'face cface))))
         (line (concat "\n" base-line (or connector ""))))
    (put-text-property 0 (length line) 'line-prefix "" line)
    (overlay-put ov 'after-string line)))

(defvar-local +polymode--redecorate-timer nil
  "Idle timer for reattempting span decoration after stale removal.")

(defun +polymode--schedule-redecorate (buf)
  "Schedule idle redecoration attempt for BUF.
Cancels any pending timer first to avoid redundant work."
  (with-current-buffer buf
    (when +polymode--redecorate-timer
      (cancel-timer +polymode--redecorate-timer))
    (setq +polymode--redecorate-timer
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq +polymode--redecorate-timer nil)
                 (+polymode-refontify-inner-spans))))))))

(defun +polymode-update-head-connectors (&rest _)
  "Update head overlay connectors after buffer changes.
If a registered :head-valid-p callback returns nil for a head overlay,
delete only that stale overlay and schedule redecoration.  Also triggers
redecorate when :count-openers reports a count diverging from head
overlay count (catches fixed-after-break scenarios)."
  (when-let* ((base (or (buffer-base-buffer) (current-buffer))))
    (with-current-buffer base
      (let* ((config (+code-fences--config))
             (head-valid-p (plist-get config :head-valid-p))
             (count-openers (plist-get config :count-openers))
             (stale nil)
             (head-count 0))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov '+polymode-head-col-c)
            (if (or (not head-valid-p)
                    (funcall head-valid-p (overlay-start ov)))
                (progn
                  (+polymode--rebuild-head-connector ov)
                  (cl-incf head-count))
              (setq stale t)
              (delete-overlay ov))))
        (when (or stale
                  (and count-openers
                       (/= head-count (funcall count-openers))))
          (setq +polymode--spans-decorated nil)
          (+polymode--schedule-redecorate base))))))

(defun +polymode--after-change-refontify (beg end _len)
  "Re-clear stale syntax properties and refontify changed region.
Undo can restore host syntax-table properties that break inner font-lock.
Also recomputes interpolation overlays for unquoted bodies via host dispatch."
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (when (buffer-base-buffer)
      (let ((sbeg (line-beginning-position (- (count-lines beg end))))
            (send (save-excursion (goto-char end) (line-end-position))))
        (with-silent-modifications
          (remove-text-properties sbeg send
                                  '(syntax-table nil syntax-multiline nil)))
        (syntax-ppss-flush-cache sbeg)
        (font-lock-flush sbeg send)))
    (let ((interp-fn (plist-get (+code-fences--config) :interpolation-fn)))
      (when interp-fn
        (dolist (ov (with-current-buffer base (overlays-in beg end)))
          (when (overlay-get ov '+polymode-unquoted)
            (let ((sbeg (overlay-start ov))
                  (send (overlay-end ov)))
              (dolist (iov (with-current-buffer base (overlays-in sbeg send)))
                (when (eq (overlay-get iov 'face) '+polymode-interpolation-face)
                  (delete-overlay iov)))
              (funcall interp-fn sbeg send base))))))))

(defun +polymode-update-header-active-state ()
  "Dim/brighten polymode headers based on whether point is in their span.
Also push point off head overlay newlines to avoid cursor landing on
synthetic header lines."
  (when-let* ((base (or (buffer-base-buffer) (current-buffer))))
    (with-current-buffer base
      (let ((pt (point)))
        ;; In evil insert state, point at end-of-line lands on the newline
        ;; after a head overlay, visually placing cursor on the synthetic
        ;; header line.  Push point back to the last real character.
        (when (and (eq (char-after pt) ?\n)
                   (not (and (bound-and-true-p evil-state)
                             (eq evil-state 'insert))))
          (dolist (ov (overlays-in (max (point-min) (1- pt)) (1+ pt)))
            (when (and (overlay-get ov '+polymode-head-col-c)
                       (= pt (overlay-end ov)))
              (goto-char (1- pt))
              (setq pt (point)))))
        ;; Update head connectors, box char faces, and tail decorations
        (+polymode--ensure-dim-face)
        (let* ((dface 'font-lock-comment-delimiter-face)
               (prefix-normal (propertize "│ " 'face dface))
               (prefix-shadow (propertize "│ " 'face '+polymode-fence-dim-face))
               (tail-normal (propertize "╰── " 'face dface))
               (tail-shadow (propertize "╰── " 'face '+polymode-fence-dim-face)))
          (dolist (ov (overlays-in (point-min) (point-max)))
            (cond
             ((overlay-get ov '+polymode-head-col-c)
              (let ((active (and (>= pt (overlay-start ov))
                                 (<= pt (overlay-get ov '+polymode-span-end)))))
                (+polymode--rebuild-head-connector ov active)))
             ((overlay-get ov '+polymode-tail)
              (let* ((span-start (overlay-get ov '+polymode-span-start))
                     (active (and span-start
                                  (>= pt span-start)
                                  (<= pt (overlay-end ov)))))
                (overlay-put ov 'before-string (if active tail-normal tail-shadow))))
             ((overlay-get ov 'line-prefix)
              (let* ((span-start (or (overlay-get ov '+polymode-span-start)
                                     (overlay-start ov)))
                     (span-end (or (overlay-get ov '+polymode-span-end)
                                   (overlay-end ov)))
                     (active (and (>= pt span-start) (<= pt span-end))))
                (overlay-put ov 'line-prefix (if active prefix-normal prefix-shadow)))))))))))

(defun +polymode-refontify-inner-spans ()
  "Clear stale host faces/syntax from inner spans, then refontify.
Must run in the polymode base buffer after mode init."
  (when (and (bound-and-true-p pm/polymode)
             (let ((base (or (buffer-base-buffer) (current-buffer))))
               (not (buffer-local-value '+polymode--spans-decorated base))))
    ;; save-excursion: pm-map-over-spans moves point; preserve it so
    ;; +polymode-update-header-active-state (called below) sees the
    ;; original point for correct dim/active state.
    (save-excursion
    ;; Cache host config in base buffer for use by after-change hooks
    (let ((base (or (buffer-base-buffer) (current-buffer))))
      (when (bound-and-true-p pm/polymode)
        (let* ((hostmode-obj (eieio-oref pm/polymode '-hostmode))
               (host-mode (and hostmode-obj (eieio-oref hostmode-obj 'mode)))
               (config (and host-mode (alist-get host-mode +code-fences-host-config))))
          (when config
            (with-current-buffer base
              (setq +code-fences--cached-config config)))))
      (with-current-buffer base
        (setq +polymode--spans-decorated t)
        (remove-overlays (point-min) (point-max) '+polymode-fence t)))
    (let (body-spans fence-spans)
      (let (last-head last-head-unquoted)
        (pm-map-over-spans
         (lambda (span)
           (pcase (nth 0 span)
             ('head
              (setq last-head t)
              (let ((unquoted-fn (plist-get (+code-fences--config) :unquoted-p)))
                (setq last-head-unquoted
                      (when unquoted-fn
                        (with-current-buffer (or (buffer-base-buffer) (current-buffer))
                          (funcall unquoted-fn (nth 1 span) (nth 2 span))))))
              (push (list 'head (nth 1 span) (nth 2 span) nil) fence-spans))
             ('body
              (let ((mode (eieio-oref (nth 3 span) :mode)))
                (push (list (nth 1 span) (nth 2 span) (current-buffer) last-head-unquoted)
                      body-spans)
                (when last-head
                  (setf (nth 3 (car fence-spans)) mode)
                  (setq last-head nil))))
             ('tail
              (when last-head (setq last-head nil))
              (push (list 'tail (nth 1 span) (nth 2 span) nil) fence-spans))))))
      (setq body-spans (nreverse body-spans))
      (setq fence-spans (nreverse fence-spans))
      (dolist (entry body-spans)
        (let ((sbeg (nth 0 entry))
              (send (nth 1 entry))
              (buf (nth 2 entry))
              (unquoted (nth 3 entry)))
          (with-current-buffer buf
            (save-restriction
              (narrow-to-region sbeg send)
              (with-silent-modifications
                (remove-text-properties sbeg send
                                        '(syntax-table nil syntax-multiline nil)))
              (let ((syntax-propertize-function nil))
                (syntax-ppss-flush-cache sbeg)
                (font-lock-unfontify-region sbeg send)
                (font-lock-fontify-region sbeg send))))
          (when unquoted
            (let ((interp-fn (plist-get (+code-fences--config) :interpolation-fn)))
              (when interp-fn
                (funcall interp-fn sbeg send (or (buffer-base-buffer) (current-buffer))))))))
      ;; Build mappings: head-start → tail-end, tail-start → head-start
      (let ((span-ends (make-hash-table :test 'eql))
            (span-starts (make-hash-table :test 'eql))
            (body-span-ranges (make-hash-table :test 'eql))
            (cur-head nil))
        (dolist (entry fence-spans)
          (pcase (nth 0 entry)
            ('head (setq cur-head (nth 1 entry)))
            ('tail (when cur-head
                     (let ((tail-end (nth 2 entry)))
                       (puthash cur-head tail-end span-ends)
                       (puthash (nth 1 entry) cur-head span-starts)
                       ;; Tag body spans belonging to this head→tail group
                       (dolist (bs body-spans)
                         (when (and (> (nth 0 bs) cur-head)
                                    (< (nth 0 bs) (nth 1 entry)))
                           (puthash (nth 0 bs) (cons cur-head tail-end)
                                    body-span-ranges))))
                     (setq cur-head nil)))))

        (let ((base (or (buffer-base-buffer) (current-buffer)))
              (dface 'font-lock-comment-delimiter-face))
          (dolist (entry fence-spans)
            (let* ((type (nth 0 entry))
                   (fbeg (nth 1 entry))
                   (fend (nth 2 entry))
                   (mode (nth 3 entry))
                   (ov (make-overlay fbeg fend base (eq type 'head))))
              (overlay-put ov 'face '+polymode-fence-face)
              (overlay-put ov '+polymode-fence t)
              (pcase type
                ('head
                 (let* ((icon (when (and mode (fboundp 'nerd-icons-icon-for-mode))
                                (nerd-icons-icon-for-mode mode)))
                        (name (when mode
                                (thread-last (symbol-name mode)
                                  (string-remove-suffix "-mode")
                                  (string-remove-suffix "-ts"))))
                        (base-line (concat (propertize "╭── " 'face dface)
                                           (when icon (concat icon " "))
                                           (propertize (or name "") 'face dface)
                                           " " (propertize "──" 'face dface)))
                        (fdim '+polymode-fence-dim-face)
                        (dim-line (concat (propertize "╭── " 'face fdim)
                                          (when icon
                                            (concat (propertize icon 'face fdim) " "))
                                          (propertize (or name "") 'face fdim)
                                          " " (propertize "──" 'face fdim))))
                   (overlay-put ov '+polymode-head-base-line base-line)
                   (overlay-put ov '+polymode-head-dim-line dim-line)
                   (overlay-put ov '+polymode-head-col-c (string-width base-line))
                   (overlay-put ov '+polymode-span-end
                                (copy-marker (gethash fbeg span-ends (overlay-end ov))))
                   (+polymode--rebuild-head-connector ov)))
                ('tail
                 (overlay-put ov '+polymode-tail t)
                 (overlay-put ov '+polymode-span-start
                              (copy-marker (gethash fbeg span-starts fbeg)))
                 (overlay-put ov 'before-string
                              (propertize "╰── " 'face dface))))))
          (let ((prefix (propertize "│ " 'face dface)))
            (dolist (entry body-spans)
              (let* ((sbeg (nth 0 entry))
                     (send (nth 1 entry))
                     (unquoted (nth 3 entry))
                     (ov (make-overlay sbeg send base))
                     (range (gethash sbeg body-span-ranges)))
                (overlay-put ov 'line-prefix prefix)
                (overlay-put ov '+polymode-fence t)
                (when range
                  (overlay-put ov '+polymode-span-start (copy-marker (car range)))
                  (overlay-put ov '+polymode-span-end (copy-marker (cdr range))))
                (when unquoted
                  (overlay-put ov '+polymode-unquoted t)))))
          )))))
  ;; Apply dim state to newly created overlays
  (+polymode-update-header-active-state)
  ;; Register hooks whenever fence overlays exist.  Runs unconditionally
  ;; because polymode mode re-init can wipe buffer-local hooks after the
  ;; initial decoration pass.
  (let ((base (or (buffer-base-buffer) (current-buffer))))
    (when (seq-some (lambda (ov) (overlay-get ov '+polymode-fence))
                    (with-current-buffer base
                      (overlays-in (point-min) (point-max))))
      (with-current-buffer base
        (add-hook 'after-change-functions #'+polymode-update-head-connectors nil t)
        (add-hook 'after-change-functions #'+polymode--after-change-refontify nil t)
        (add-hook 'post-command-hook #'+polymode-update-header-active-state nil t)))))

(defun +polymode--theme-changed-h ()
  "Invalidate dim-face cache and refresh overlays in all polymode buffers."
  (setq +polymode--fence-dim-hex nil
        +polymode--fence-dim-last-fg nil
        +polymode--fence-dim-last-bg nil)
  (dolist (buf (buffer-list))
    (when (buffer-local-value '+polymode--spans-decorated buf)
      (with-current-buffer buf
        (+polymode-update-header-active-state)))))

(add-hook '+theme-changed-hook #'+polymode--theme-changed-h)

(provide '+code-fences)

;;; +code-fences.el ends here
