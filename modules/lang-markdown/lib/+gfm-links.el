;;; +gfm-links.el --- Overlay decoration for GFM links -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that decorates Markdown / GFM links with per-window
;; overlays, replacing the bracket scaffolding with a clean reading
;; surface:
;;
;;   [Anthropic](https://anthropic.com)   ->   Anthropic <icon>
;;
;; The title side (`[title]', brackets included) is display-replaced
;; with the title text in `markdown-link-face'.  The URL side (`(url)',
;; `[label]', the autolink span, …) is display-replaced with a single
;; nerd-icons glyph resolved from the target.  Image links, reference
;; definition lines, and footnote markers are deliberately left raw.
;;
;; Mirrors the other GFM decorators in this module:
;;
;; - Overlays are created per-window (via the `window' overlay
;;   property) so two windows showing the same buffer decorate and
;;   reveal independently.
;; - A post-command reveal exposes the raw source of the whole link
;;   (both the title-side and url-side overlays, matched by a shared
;;   `gfm-links-id') while point is inside it.
;; - Buffer edits arm a debounced rebuild.  Discovery widens, so the
;;   rebuild is narrowing-resilient.
;;
;; markdown-mode's built-in `markdown-hide-urls' compose-region URL
;; collapse is suppressed while the mode is on, via `:around' advice on
;; `markdown-fontify-inline-links' / `markdown-fontify-reference-links'
;; gated by the buffer-local mode variable.
;;
;; Reference links resolve through a buffer-local definition alist.
;; `RET' on a decorated link follows the URL; `M-.' jumps to the
;; `[label]:' definition line via an `xref' backend; eldoc surfaces the
;; resolved URL.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'xref)
(require 'eldoc)
(require 'markdown-mode)
(require '+gfm-block-borders)
(require 'nerd-icons nil t)

(defvar gfm-links-mode)

(defgroup gfm-links nil
  "Overlay decoration for GitHub Flavored Markdown links."
  :group 'markdown-faces)

(defcustom gfm-links-title-face 'markdown-link-face
  "Face used for the display text of a decorated link's title side."
  :type 'face
  :group 'gfm-links)

(defvar-keymap gfm-links--overlay-keymap
  :doc "Keymap active inside a decorated link's title-side overlay."
  "RET" #'gfm-links-follow-link-at-point)

;;; Buffer-local state

(defvar-local gfm-links--ref-def-alist nil
  "Buffer-local alist mapping a downcased reference label to its definition.
Each value is (URL TITLE-ATTR DEF-POS): the resolved URL string, the
optional title attribute, and the buffer position of the `[label]:'
definition line.  Recomputed at the start of every rebuild; the first
definition for a duplicate label wins.")

(defvar-local gfm-links--overlays nil
  "All gfm-links overlays currently in this buffer.")

(defvar-local gfm-links--hidden-ovs nil
  "Revealable gfm-links overlays whose display is currently suppressed.")

(defvar-local gfm-links--rebuild-timer nil
  "Idle timer for the debounced overlay rebuild.")

(defvar-local gfm-links--id-counter 0
  "Monotonic counter backing `gfm-links--next-id'.")

(defvar-local gfm-links--watching nil
  "Non-nil in buffers that opted into `markdown-hide-urls' tracking.
Set by `gfm-links--maybe-enable'.  The global `markdown-hide-urls'
variable watcher only toggles the mode in buffers carrying this flag,
which keeps the watcher effectively buffer-local: a plain
`markdown-mode' buffer that never ran `gfm-links--maybe-enable' is
left alone.")

(defun gfm-links--next-id ()
  "Return a fresh per-link identifier, unique within this buffer.
The title-side and url-side overlays of one link share this id so the
reveal hook can find the partner overlay."
  (cl-incf gfm-links--id-counter))

(defconst gfm-links--registry
  (gfm-block-borders-registry-for
   'gfm-links 'gfm-links--overlays 'gfm-links--hidden-ovs)
  "Shared overlay-registry context for gfm-links.")

(defsubst gfm-links--remove-overlays (&optional beg end)
  "Remove all gfm-links overlays between BEG and END (full reset when nil)."
  (gfm-block-borders--remove-overlays gfm-links--registry beg end))

;;; Link records

(cl-defstruct (gfm-links--link
               (:constructor gfm-links--make-link)
               (:copier nil))
  "One decorated link.
KIND is one of `inline', `reference', `autolink', `bare-url', `wiki'.
TBEG/TEND bound the title-side overlay; LABEL is its display string.
UBEG/UEND bound the url-side overlay.  URL is the resolved target.
TITLE-ATTR is the inline title attribute, when present.  REF-LABEL is
the reference label (reference links only); REF-DEF-POS is the buffer
position of its `[label]:' definition line."
  kind tbeg tend label ubeg uend url title-attr ref-label ref-def-pos)

;;; Reference-definition alist

(defun gfm-links--build-ref-def-alist ()
  "Recompute `gfm-links--ref-def-alist' from the whole buffer.
Scans `markdown-regex-reference-definition'; the first definition for a
duplicate label wins, matching `markdown-reference-definition'."
  (let (alist)
    (save-restriction
      (widen)
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward markdown-regex-reference-definition nil t)
            (let* ((label (downcase (match-string-no-properties 2)))
                   (url (string-trim (match-string-no-properties 5)))
                   (title (gfm-links--strip-title-quotes
                           (match-string-no-properties 6)))
                   (pos (match-beginning 0)))
              (unless (assoc label alist)
                (push (list label url title pos) alist)))))))
    (setq gfm-links--ref-def-alist (nreverse alist))))

(defun gfm-links--strip-title-quotes (raw)
  "Return the inner text of a quoted title attribute RAW, or nil.
Wrapped in `save-match-data' so callers can read RAW out of regexp
match data without the inner `string-match' clobbering it."
  (and raw
       (save-match-data
         (and (string-match "\"\\(.*\\)\"" raw)
              (match-string 1 raw)))))

(defun gfm-links--resolve-ref (label)
  "Return (URL TITLE-ATTR DEF-POS) for reference LABEL, or nil if undefined.
LABEL is matched case-insensitively, matching markdown-mode."
  (when (and label (not (string-empty-p label)))
    (cdr (assoc (downcase label) gfm-links--ref-def-alist))))

;;; Icon resolution

(defun gfm-links--call-nerd (fn arg)
  "Call nerd-icons FN with ARG when it is available; nil otherwise."
  (and (fboundp fn) (ignore-errors (funcall fn arg))))

(defun gfm-links--icon-for-target (url)
  "Return a single nerd-icons glyph for URL, deferring entirely to nerd-icons.
http(s) URLs, same-document anchors, and other absolute schemes resolve
via `nerd-icons-icon-for-url'; relative paths and `file:' URLs resolve
via `nerd-icons-icon-for-file' on the basename.  No `:height' override
is passed.  Returns nil when nerd-icons is unavailable."
  (when (and url (not (string-empty-p url)))
    (cond
     ((string-match-p (rx bos (or "http://" "https://")) url)
      (gfm-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "#" url)
      (gfm-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "file:" url)
      (gfm-links--call-nerd
       #'nerd-icons-icon-for-file
       (file-name-nondirectory (string-remove-prefix "file:" url))))
     ((string-match-p (rx bos (+ (any "a-zA-Z0-9.+-")) ":") url)
      (gfm-links--call-nerd #'nerd-icons-icon-for-url url))
     (t
      (gfm-links--call-nerd #'nerd-icons-icon-for-file
                            (file-name-nondirectory url))))))

(defun gfm-links--label-for-naked-url (url)
  "Return the visible label for an autolink or bare URL with target URL.
The host portion is used when the URL parses to one; otherwise the URL
itself is the label."
  (or (ignore-errors (url-host (url-generic-parse-url url)))
      url))

;;; Link-shape discovery

(defun gfm-links--ref-def-line-ranges ()
  "Return (BEG . END) line ranges of every reference-definition line.
Records starting inside one of these ranges are not decorated."
  ;; `markdown-regex-reference-definition' can match past the trailing
  ;; newline (its `\\s *' run is newline-permissive), so derive the line
  ;; range from `match-beginning' rather than point after the search.
  (let (ranges)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward markdown-regex-reference-definition nil t)
          (goto-char (match-beginning 0))
          (push (cons (line-beginning-position) (line-end-position)) ranges)
          (forward-line 1))))
    (nreverse ranges)))

(defun gfm-links--pos-in-ranges-p (pos ranges)
  "Non-nil if POS lies within any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-links--scan-inline (beg end)
  "Return inline-link records found between BEG and END.
Image links (`![alt](url)') are rejected via the leading-bang group."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-link-inline end t)
          (unless (match-beginning 1)   ; leading `!' => image, skip
            (let ((label (match-string-no-properties 3))
                  (url (string-trim (or (match-string-no-properties 6) "")))
                  (title (gfm-links--strip-title-quotes
                          (match-string-no-properties 7)))
                  (tbeg (match-beginning 2)) (tend (match-end 4))
                  (ubeg (match-beginning 5)) (uend (match-end 8)))
              (push (gfm-links--make-link
                     :kind 'inline
                     :tbeg tbeg :tend tend
                     :label (if (string-empty-p label) url label)
                     :ubeg ubeg :uend uend
                     :url url :title-attr title)
                    records))))))
    (nreverse records)))

(defun gfm-links--scan-reference (beg end)
  "Return full/collapsed reference-link records found between BEG and END.
Image variants are rejected; links whose label has no definition are
dropped (the raw `[title][label]' shows through unchanged)."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-link-reference end t)
          (unless (match-beginning 1)   ; leading `!' => image, skip
            (let* ((text (match-string-no-properties 3))
                   (raw-label (match-string-no-properties 6))
                   (ref-label (if (string-empty-p raw-label) text raw-label))
                   (entry (gfm-links--resolve-ref ref-label)))
              (when entry
                (push (gfm-links--make-link
                       :kind 'reference
                       :tbeg (match-beginning 2) :tend (match-end 4)
                       :label (if (string-empty-p text) ref-label text)
                       :ubeg (match-beginning 5) :uend (match-end 7)
                       :url (nth 0 entry)
                       :title-attr (nth 1 entry)
                       :ref-label ref-label
                       :ref-def-pos (nth 2 entry))
                      records)))))))
    (nreverse records)))

(defconst gfm-links--shortcut-re
  (rx "[" (group (+ (not (any "[]")))) "]")
  "Regexp matching a potential shortcut reference `[label]'.")

(defun gfm-links--scan-shortcut (beg end ref-def-ranges)
  "Return shortcut reference-link records (`[label]') between BEG and END.
Only `[label]' spans with a matching definition are decorated.  Spans
that are part of an inline or full/collapsed reference link, footnote
markers, image links, and reference-definition lines are skipped.
REF-DEF-RANGES is the result of `gfm-links--ref-def-line-ranges'."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward gfm-links--shortcut-re end t)
          (let* ((mbeg (match-beginning 0))
                 (mend (match-end 0))
                 (label (match-string-no-properties 1))
                 (before (char-before mbeg))
                 (after (char-after mend)))
            (when (and (not (string-prefix-p "^" label)) ; footnote
                       ;; `[[' wiki opening / `]' second bracket / `!' image.
                       (not (memq before '(?\[ ?\] ?!)))
                       ;; `(' inline / `[' full ref / `:' definition line.
                       (not (memq after '(?\( ?\[ ?:)))
                       (not (gfm-links--pos-in-ranges-p mbeg ref-def-ranges)))
              (let ((entry (gfm-links--resolve-ref label)))
                (when entry
                  (push (gfm-links--make-link
                         :kind 'reference
                         :tbeg mbeg :tend mend
                         :label label
                         ;; No second bracket pair: hang the icon off
                         ;; the closing `]'.
                         :ubeg (1- mend) :uend mend
                         :url (nth 0 entry)
                         :title-attr (nth 1 entry)
                         :ref-label label
                         :ref-def-pos (nth 2 entry))
                        records))))))))
    (nreverse records)))

(defun gfm-links--naked-record (kind beg span-end url)
  "Build a KIND record for a naked URL spanning BEG..SPAN-END with target URL.
The title side covers all but the last column (host label); the url
side is the final column (icon).  Falls back to a single-column title
when the span is too short to split."
  (let* ((label (gfm-links--label-for-naked-url url))
         (split (if (> (- span-end beg) 1) (1- span-end) span-end)))
    (gfm-links--make-link
     :kind kind
     :tbeg beg :tend split :label label
     :ubeg split :uend span-end
     :url url)))

(defun gfm-links--scan-autolinks (beg end)
  "Return autolink records (`<scheme:…>') found between BEG and END."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-angle-uri end t)
          (push (gfm-links--naked-record
                 'autolink (match-beginning 0) (match-end 0)
                 (match-string-no-properties 2))
                records))))
    (nreverse records)))

(defun gfm-links--scan-bare-urls (beg end)
  "Return GFM bare-URL records (`https?://…') found between BEG and END."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-uri end t)
          (push (gfm-links--naked-record
                 'bare-url (match-beginning 1) (match-end 1)
                 (match-string-no-properties 1))
                records))))
    (nreverse records)))

(defun gfm-links--scan-wiki (beg end)
  "Return wiki-link records found between BEG and END.
Only scans when `markdown-enable-wiki-links' is non-nil."
  (when markdown-enable-wiki-links
    (let (records)
      (save-excursion
        (save-match-data
          (goto-char beg)
          (while (re-search-forward markdown-regex-wiki-link end t)
            (let* ((sbeg (match-beginning 2))   ; opening `[['
                   (send (match-end 6))         ; closing `]]'
                   (label (markdown-wiki-link-alias))
                   (page (markdown-wiki-link-link))
                   (target (markdown-convert-wiki-link-to-filename page))
                   (split (if (> (- send sbeg) 1) (1- send) send)))
              (push (gfm-links--make-link
                     :kind 'wiki
                     :tbeg sbeg :tend split :label label
                     :ubeg split :uend send
                     :url target)
                    records)))))
      (nreverse records))))

(defun gfm-links--record-span (record)
  "Return the full (BEG . END) buffer span covered by RECORD."
  (cons (min (gfm-links--link-tbeg record) (gfm-links--link-ubeg record))
        (max (gfm-links--link-tend record) (gfm-links--link-uend record))))

(defun gfm-links--blocks-in-range (beg end)
  "Return the gfm-links link records between BEG and END, in buffer order.
Records are collected by shape in priority order; a later record whose
span overlaps an already-claimed span is dropped, so a bare URL inside
an inline link's target, or a `[label]' that is really a full
reference link's text, is not double-decorated.  Reference-definition
lines are excluded entirely."
  (let* ((ref-def-ranges (gfm-links--ref-def-line-ranges))
         (claimed nil)
         (kept nil))
    (dolist (record (append (gfm-links--scan-inline beg end)
                            (gfm-links--scan-reference beg end)
                            (gfm-links--scan-wiki beg end)
                            (gfm-links--scan-autolinks beg end)
                            (gfm-links--scan-bare-urls beg end)
                            (gfm-links--scan-shortcut
                             beg end ref-def-ranges)))
      (let ((span (gfm-links--record-span record)))
        (unless (or (gfm-links--pos-in-ranges-p (car span) ref-def-ranges)
                    (cl-some (lambda (c)
                               (gfm-block-borders--region-overlaps-p span c))
                             claimed))
          (push span claimed)
          (push record kept))))
    (sort kept (lambda (a b)
                 (< (gfm-links--link-tbeg a) (gfm-links--link-tbeg b))))))

;;; Overlay construction

(defun gfm-links--make-overlay (beg end window side record id)
  "Create one gfm-links overlay over [BEG, END) for WINDOW.
SIDE is `title' or `url'.  RECORD is the `gfm-links--link' it belongs
to; ID is the shared per-link identifier.  The overlay carries the
link's resolved metadata so reveal, RET, eldoc, and the xref backend
can read it without re-parsing."
  (let ((display
         (if (eq side 'title)
             (propertize (gfm-links--link-label record)
                         'face gfm-links-title-face)
           (or (gfm-links--icon-for-target (gfm-links--link-url record))
               ""))))
    (apply #'gfm-block-borders--make-display
           gfm-links--registry beg end window
           'gfm-links-revealable t
           'gfm-links-id id
           'gfm-links-side side
           'gfm-links-kind (gfm-links--link-kind record)
           'gfm-links-url (gfm-links--link-url record)
           'gfm-links-title-attr (gfm-links--link-title-attr record)
           'gfm-links-ref-label (gfm-links--link-ref-label record)
           'gfm-links-ref-def-pos (gfm-links--link-ref-def-pos record)
           'display display
           'evaporate t
           (when (eq side 'title)
             (list 'keymap gfm-links--overlay-keymap)))))

(defun gfm-links--make-title-overlay (record window id)
  "Create RECORD's title-side overlay for WINDOW with shared id ID."
  (gfm-links--make-overlay (gfm-links--link-tbeg record)
                           (gfm-links--link-tend record)
                           window 'title record id))

(defun gfm-links--make-url-overlay (record window id)
  "Create RECORD's url-side overlay for WINDOW with shared id ID."
  (gfm-links--make-overlay (gfm-links--link-ubeg record)
                           (gfm-links--link-uend record)
                           window 'url record id))

(defun gfm-links--decorate-link (record window)
  "Create the title-side and url-side overlays for RECORD in WINDOW.
Degenerate records (empty title span) are skipped."
  (when (< (gfm-links--link-tbeg record) (gfm-links--link-tend record))
    (let ((id (gfm-links--next-id)))
      (gfm-links--make-title-overlay record window id)
      (when (< (gfm-links--link-ubeg record) (gfm-links--link-uend record))
        (gfm-links--make-url-overlay record window id)))))

;;; Rebuild

(defun gfm-links--rebuild ()
  "Remove and recreate every gfm-links overlay in the buffer.
Discovery and overlay creation widen, so the rebuild is
narrowing-resilient: a rebuild within a narrowed region produces the
same overlay set as a widened rebuild.  The reference-definition alist
is recomputed first so reference links resolve against current state."
  (save-restriction
    (widen)
    (save-excursion
      (gfm-links--remove-overlays)
      (gfm-links--build-ref-def-alist)
      (let ((blocks (gfm-links--blocks-in-range (point-min) (point-max)))
            (windows (or (gfm-block-borders--display-windows) (list nil))))
        (dolist (window windows)
          (dolist (record blocks)
            (gfm-links--decorate-link record window)))))))

(defun gfm-links--schedule-rebuild (&rest _)
  "Arm the debounced rebuild timer after a buffer change."
  (unless (buffer-base-buffer)
    (gfm-block-borders--arm-rebuild-timer
     'gfm-links--rebuild-timer 'gfm-links-mode #'gfm-links--rebuild)))

;;; Cursor-driven reveal (per-window, whole-link)

(defun gfm-links--link-id-at (pos window)
  "Return the `gfm-links-id' of a revealable overlay covering POS in WINDOW.
Only overlays whose `window' property is nil or WINDOW are considered."
  (cl-loop for ov in (overlays-in pos (min (1+ pos) (point-max)))
           when (and (overlay-get ov 'gfm-links-revealable)
                     (let ((w (overlay-get ov 'window)))
                       (or (null w) (eq w window))))
           return (overlay-get ov 'gfm-links-id)))

(defun gfm-links--restore-overlay (ov)
  "Restore OV's suppressed display property."
  (when (overlay-buffer ov)
    (overlay-put ov 'display (overlay-get ov 'gfm-links-saved-display))
    (overlay-put ov 'gfm-links-saved-display nil)))

(defun gfm-links--reveal ()
  "Reveal the whole link under point in the selected window.
Suppresses `display' on both the title-side and url-side overlays
\(matched by `gfm-links-id') of the link at point, and restores any
previously-revealed link once point leaves it.  Per-window: overlays
scoped to another window are never touched."
  (let* ((pos (point))
         (win (selected-window))
         (active-id (gfm-links--link-id-at pos win)))
    (setq gfm-links--hidden-ovs
          (cl-loop for ov in gfm-links--hidden-ovs
                   if (and (overlay-buffer ov)
                           active-id
                           (eq (overlay-get ov 'gfm-links-id) active-id)
                           (let ((w (overlay-get ov 'window)))
                             (or (null w) (eq w win))))
                   collect ov
                   else do (gfm-links--restore-overlay ov)))
    (when active-id
      (dolist (ov gfm-links--overlays)
        (when (and (overlay-buffer ov)
                   (eq (overlay-get ov 'gfm-links-id) active-id)
                   (overlay-get ov 'gfm-links-revealable)
                   (overlay-get ov 'display)
                   (let ((w (overlay-get ov 'window)))
                     (or (null w) (eq w win)))
                   (not (memq ov gfm-links--hidden-ovs)))
          (overlay-put ov 'gfm-links-saved-display (overlay-get ov 'display))
          (overlay-put ov 'display nil)
          (push ov gfm-links--hidden-ovs))))))

;;; Suppression of the built-in compose path

(define-advice markdown-fontify-inline-links
    (:around (orig &rest args) gfm-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose-region branch under `gfm-links-mode'.
The body still runs — faces apply, properties propagate — only the URL
glyph composition is suppressed, because the gfm-links overlays own the
link's appearance.  Inert in buffers where the mode is off."
  (if (bound-and-true-p gfm-links-mode)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

(define-advice markdown-fontify-reference-links
    (:around (orig &rest args) gfm-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose-region branch under `gfm-links-mode'.
See `markdown-fontify-inline-links@gfm-links-suppress-compose'."
  (if (bound-and-true-p gfm-links-mode)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

;;; RET / follow-link

(defun gfm-links--overlay-at-point ()
  "Return a gfm-links overlay covering point in the selected window, or nil."
  (let ((win (selected-window)))
    (cl-find-if (lambda (ov)
                  (and (overlay-get ov 'gfm-links-revealable)
                       (let ((w (overlay-get ov 'window)))
                         (or (null w) (eq w win)))))
                (overlays-in (point) (min (1+ (point)) (point-max))))))

(defun gfm-links-follow-link-at-point ()
  "Follow the decorated link at point via `markdown--browse-url'.
Bound to `RET' through the title-side overlay's `keymap' property, so
off any decorated link `RET' keeps its global binding."
  (interactive)
  (let* ((ov (gfm-links--overlay-at-point))
         (url (and ov (overlay-get ov 'gfm-links-url))))
    (if (and url (not (string-empty-p url)))
        (markdown--browse-url url)
      (user-error "Point is not on a decorated link"))))

;;; Reference goto-definition via xref

(defun gfm-links--xref-backend ()
  "Return the gfm-links xref backend symbol when point is on a reference link.
Returns nil otherwise so other xref backends are consulted."
  (let ((ov (gfm-links--overlay-at-point)))
    (and ov
         (overlay-get ov 'gfm-links-ref-def-pos)
         'gfm-links)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gfm-links)))
  (let ((ov (gfm-links--overlay-at-point)))
    (and ov (overlay-get ov 'gfm-links-ref-label))))

(cl-defmethod xref-backend-definitions ((_backend (eql gfm-links)) identifier)
  (let ((entry (gfm-links--resolve-ref identifier)))
    (when entry
      (list (xref-make
             identifier
             (xref-make-buffer-location (current-buffer) (nth 2 entry)))))))

;;; Eldoc URL exposure

(defun gfm-links--eldoc-function (&rest _)
  "Return the resolved URL of the decorated link at point for eldoc.
Includes the inline title attribute when present.  Returns nil off any
decorated link so other eldoc providers are not blocked."
  (let* ((ov (gfm-links--overlay-at-point))
         (url (and ov (overlay-get ov 'gfm-links-url)))
         (title (and ov (overlay-get ov 'gfm-links-title-attr))))
    (when (and url (not (string-empty-p url)))
      (if (and title (not (string-empty-p title)))
          (format "%s — %s" url title)
        url))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-links-mode
  "Decorate Markdown links with per-window overlays.
Replaces the bracket scaffolding of inline, reference, autolink,
bare-URL, and wiki links with a title label and a host-aware icon.
See the file commentary for the full behaviour."
  :lighter " gfm-ln"
  (if gfm-links-mode
      (progn
        (gfm-links--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-links--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-links--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-links--reveal nil t)
        (add-hook 'xref-backend-functions #'gfm-links--xref-backend nil t)
        (add-hook 'eldoc-documentation-functions
                  #'gfm-links--eldoc-function nil t))
    (remove-hook 'after-change-functions #'gfm-links--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-links--schedule-rebuild t)
    (remove-hook 'post-command-hook #'gfm-links--reveal t)
    (remove-hook 'xref-backend-functions #'gfm-links--xref-backend t)
    (remove-hook 'eldoc-documentation-functions
                 #'gfm-links--eldoc-function t)
    (when (timerp gfm-links--rebuild-timer)
      (cancel-timer gfm-links--rebuild-timer))
    (setq gfm-links--rebuild-timer nil
          gfm-links--ref-def-alist nil)
    (gfm-links--remove-overlays)))

;;; markdown-hide-urls integration

(defun gfm-links--watch-hide-urls (_symbol newval operation where)
  "Variable watcher: track `markdown-hide-urls' changes into `gfm-links-mode'.
Enabling/disabling stays independent of the variable — toggling the
variable simply follows through to the mode in the affected buffer
\(WHERE, or the current buffer for a global set)."
  (when (eq operation 'set)
    (let ((buf (if (bufferp where) where (current-buffer))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and gfm-links--watching (derived-mode-p 'markdown-mode))
            (if newval
                (unless gfm-links-mode (gfm-links-mode 1))
              (when gfm-links-mode (gfm-links-mode -1)))))))))

;;;###autoload
(defun gfm-links--maybe-enable ()
  "Enable `gfm-links-mode' when `markdown-hide-urls' is on, and track it.
Wired into `markdown-mode-hook' / `gfm-mode-hook'.  Installs a
buffer-local watcher on `markdown-hide-urls' so later changes to the
variable toggle the mode."
  (setq gfm-links--watching t)
  (add-variable-watcher 'markdown-hide-urls #'gfm-links--watch-hide-urls)
  (when (bound-and-true-p markdown-hide-urls)
    (gfm-links-mode 1)))

(provide '+gfm-links)

;;; +gfm-links.el ends here
