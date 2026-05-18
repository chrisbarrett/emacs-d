;;; gfm-pretty-links.el --- Overlay decoration for GFM links -*- lexical-binding: t; -*-

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
;;   `gfm-pretty-links-id') while point is inside it.
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
(require 'gfm-pretty-borders)
(require 'nerd-icons nil t)

(defvar gfm-pretty-links-mode)

(defgroup gfm-pretty-links nil
  "Overlay decoration for GitHub Flavored Markdown links."
  :group 'markdown-faces)

(defcustom gfm-pretty-links-title-face 'markdown-link-face
  "Face used for the display text of a decorated link's title side."
  :type 'face
  :group 'gfm-pretty-links)

(defvar-keymap gfm-pretty-links--overlay-keymap
  :doc "Keymap active inside a decorated link's title-side overlay."
  "RET" #'gfm-pretty-links-follow-link-at-point)

;;; Buffer-local state

(defvar-local gfm-pretty-links--ref-def-alist nil
  "Buffer-local alist mapping a downcased reference label to its definition.
Each value is (URL TITLE-ATTR DEF-POS): the resolved URL string, the
optional title attribute, and the buffer position of the `[label]:'
definition line.  Recomputed at the start of every rebuild; the first
definition for a duplicate label wins.")

(defvar-local gfm-pretty-links--overlays nil
  "All gfm-pretty-links overlays currently in this buffer.")

(defvar-local gfm-pretty-links--hidden-ovs nil
  "Revealable gfm-pretty-links overlays whose display is currently suppressed.")

(defvar-local gfm-pretty-links--rebuild-timer nil
  "Idle timer for the debounced overlay rebuild.")

(defvar-local gfm-pretty-links--id-counter 0
  "Monotonic counter backing `gfm-pretty-links--next-id'.")

(defvar-local gfm-pretty-links--watching nil
  "Non-nil in buffers that opted into `markdown-hide-urls' tracking.
Set by `gfm-pretty-links--maybe-enable'.  The global `markdown-hide-urls'
variable watcher only toggles the mode in buffers carrying this flag,
which keeps the watcher effectively buffer-local: a plain
`markdown-mode' buffer that never ran `gfm-pretty-links--maybe-enable' is
left alone.")

(defun gfm-pretty-links--next-id ()
  "Return a fresh per-link identifier, unique within this buffer.
The title-side and url-side overlays of one link share this id so the
reveal hook can find the partner overlay."
  (cl-incf gfm-pretty-links--id-counter))

(defconst gfm-pretty-links--registry
  (gfm-pretty--registry-for
   'gfm-pretty-links 'gfm-pretty-links--overlays 'gfm-pretty-links--hidden-ovs)
  "Shared overlay-registry context for gfm-pretty-links.")

(defsubst gfm-pretty-links--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-links overlays between BEG and END (full reset when nil)."
  (gfm-pretty--remove-overlays gfm-pretty-links--registry beg end))

;;; Link records

(cl-defstruct (gfm-pretty-links--link
               (:constructor gfm-pretty-links--make-link)
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

(defun gfm-pretty-links--build-ref-def-alist ()
  "Recompute `gfm-pretty-links--ref-def-alist' from the whole buffer.
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
                   (title (gfm-pretty-links--strip-title-quotes
                           (match-string-no-properties 6)))
                   (pos (match-beginning 0)))
              (unless (assoc label alist)
                (push (list label url title pos) alist)))))))
    (setq gfm-pretty-links--ref-def-alist (nreverse alist))))

(defun gfm-pretty-links--strip-title-quotes (raw)
  "Return the inner text of a quoted title attribute RAW, or nil.
Wrapped in `save-match-data' so callers can read RAW out of regexp
match data without the inner `string-match' clobbering it."
  (and raw
       (save-match-data
         (and (string-match "\"\\(.*\\)\"" raw)
              (match-string 1 raw)))))

(defun gfm-pretty-links--resolve-ref (label)
  "Return (URL TITLE-ATTR DEF-POS) for reference LABEL, or nil if undefined.
LABEL is matched case-insensitively, matching markdown-mode."
  (when (and label (not (string-empty-p label)))
    (cdr (assoc (downcase label) gfm-pretty-links--ref-def-alist))))

;;; Icon resolution

(defun gfm-pretty-links--call-nerd (fn arg)
  "Call nerd-icons FN with ARG when it is available; nil otherwise."
  (and (fboundp fn) (ignore-errors (funcall fn arg))))

(defun gfm-pretty-links--icon-for-target (url)
  "Return a single nerd-icons glyph for URL, deferring entirely to nerd-icons.
http(s) URLs, same-document anchors, and other absolute schemes resolve
via `nerd-icons-icon-for-url'; relative paths and `file:' URLs resolve
via `nerd-icons-icon-for-file' on the basename.  No `:height' override
is passed.  Returns nil when nerd-icons is unavailable."
  (when (and url (not (string-empty-p url)))
    (cond
     ((string-match-p (rx bos (or "http://" "https://")) url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "#" url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "file:" url)
      (gfm-pretty-links--call-nerd
       #'nerd-icons-icon-for-file
       (file-name-nondirectory (string-remove-prefix "file:" url))))
     ((string-match-p (rx bos (+ (any "a-zA-Z0-9.+-")) ":") url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     (t
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-file
                            (file-name-nondirectory url))))))

(defun gfm-pretty-links--label-for-naked-url (url)
  "Return the visible label for an autolink or bare URL with target URL.
The host portion is used when the URL parses to one; otherwise the URL
itself is the label."
  (or (ignore-errors (url-host (url-generic-parse-url url)))
      url))

;;; Link-shape discovery

(defun gfm-pretty-links--ref-def-line-ranges ()
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

(defun gfm-pretty-links--pos-in-ranges-p (pos ranges)
  "Non-nil if POS lies within any (BEG . END) range in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (<= pos (cdr r)))) ranges))

(defun gfm-pretty-links--scan-inline (beg end)
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
                  (title (gfm-pretty-links--strip-title-quotes
                          (match-string-no-properties 7)))
                  (tbeg (match-beginning 2)) (tend (match-end 4))
                  (ubeg (match-beginning 5)) (uend (match-end 8)))
              (push (gfm-pretty-links--make-link
                     :kind 'inline
                     :tbeg tbeg :tend tend
                     :label (if (string-empty-p label) url label)
                     :ubeg ubeg :uend uend
                     :url url :title-attr title)
                    records))))))
    (nreverse records)))

(defun gfm-pretty-links--scan-reference (beg end)
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
                   (entry (gfm-pretty-links--resolve-ref ref-label)))
              (when entry
                (push (gfm-pretty-links--make-link
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

(defconst gfm-pretty-links--shortcut-re
  (rx "[" (group (+ (not (any "[]")))) "]")
  "Regexp matching a potential shortcut reference `[label]'.")

(defun gfm-pretty-links--scan-shortcut (beg end ref-def-ranges)
  "Return shortcut reference-link records (`[label]') between BEG and END.
Only `[label]' spans with a matching definition are decorated.  Spans
that are part of an inline or full/collapsed reference link, footnote
markers, image links, and reference-definition lines are skipped.
REF-DEF-RANGES is the result of `gfm-pretty-links--ref-def-line-ranges'."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward gfm-pretty-links--shortcut-re end t)
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
                       (not (gfm-pretty-links--pos-in-ranges-p mbeg ref-def-ranges)))
              (let ((entry (gfm-pretty-links--resolve-ref label)))
                (when entry
                  (push (gfm-pretty-links--make-link
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

(defun gfm-pretty-links--naked-record (kind beg span-end url)
  "Build a KIND record for a naked URL spanning BEG..SPAN-END with target URL.
The title side covers all but the last column (host label); the url
side is the final column (icon).  Falls back to a single-column title
when the span is too short to split."
  (let* ((label (gfm-pretty-links--label-for-naked-url url))
         (split (if (> (- span-end beg) 1) (1- span-end) span-end)))
    (gfm-pretty-links--make-link
     :kind kind
     :tbeg beg :tend split :label label
     :ubeg split :uend span-end
     :url url)))

(defun gfm-pretty-links--scan-autolinks (beg end)
  "Return autolink records (`<scheme:…>') found between BEG and END."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-angle-uri end t)
          (push (gfm-pretty-links--naked-record
                 'autolink (match-beginning 0) (match-end 0)
                 (match-string-no-properties 2))
                records))))
    (nreverse records)))

(defun gfm-pretty-links--scan-bare-urls (beg end)
  "Return GFM bare-URL records (`https?://…') found between BEG and END."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-uri end t)
          (push (gfm-pretty-links--naked-record
                 'bare-url (match-beginning 1) (match-end 1)
                 (match-string-no-properties 1))
                records))))
    (nreverse records)))

(defun gfm-pretty-links--scan-wiki (beg end)
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
              (push (gfm-pretty-links--make-link
                     :kind 'wiki
                     :tbeg sbeg :tend split :label label
                     :ubeg split :uend send
                     :url target)
                    records)))))
      (nreverse records))))

(defun gfm-pretty-links--record-span (record)
  "Return the full (BEG . END) buffer span covered by RECORD."
  (cons (min (gfm-pretty-links--link-tbeg record) (gfm-pretty-links--link-ubeg record))
        (max (gfm-pretty-links--link-tend record) (gfm-pretty-links--link-uend record))))

(defun gfm-pretty-links--blocks-in-range (beg end)
  "Return the gfm-pretty-links link records between BEG and END, in buffer order.
Records are collected by shape in priority order; a later record whose
span overlaps an already-claimed span is dropped, so a bare URL inside
an inline link's target, or a `[label]' that is really a full
reference link's text, is not double-decorated.  Reference-definition
lines are excluded entirely."
  (let* ((ref-def-ranges (gfm-pretty-links--ref-def-line-ranges))
         (claimed nil)
         (kept nil))
    (dolist (record (append (gfm-pretty-links--scan-inline beg end)
                            (gfm-pretty-links--scan-reference beg end)
                            (gfm-pretty-links--scan-wiki beg end)
                            (gfm-pretty-links--scan-autolinks beg end)
                            (gfm-pretty-links--scan-bare-urls beg end)
                            (gfm-pretty-links--scan-shortcut
                             beg end ref-def-ranges)))
      (let ((span (gfm-pretty-links--record-span record)))
        (unless (or (gfm-pretty-links--pos-in-ranges-p (car span) ref-def-ranges)
                    (cl-some (lambda (c)
                               (gfm-pretty--region-overlaps-p span c))
                             claimed))
          (push span claimed)
          (push record kept))))
    (sort kept (lambda (a b)
                 (< (gfm-pretty-links--link-tbeg a) (gfm-pretty-links--link-tbeg b))))))

;;; Overlay construction

(defun gfm-pretty-links--make-overlay (beg end window side record id)
  "Create one gfm-pretty-links overlay over [BEG, END) for WINDOW.
SIDE is `title' or `url'.  RECORD is the `gfm-pretty-links--link' it belongs
to; ID is the shared per-link identifier.  The overlay carries the
link's resolved metadata so reveal, RET, eldoc, and the xref backend
can read it without re-parsing."
  (let ((display
         (if (eq side 'title)
             (propertize (gfm-pretty-links--link-label record)
                         'face gfm-pretty-links-title-face)
           (or (gfm-pretty-links--icon-for-target (gfm-pretty-links--link-url record))
               ""))))
    (apply #'gfm-pretty--make-display
           gfm-pretty-links--registry beg end window
           'gfm-pretty-links-revealable t
           'gfm-pretty-links-id id
           'gfm-pretty-links-side side
           'gfm-pretty-links-kind (gfm-pretty-links--link-kind record)
           'gfm-pretty-links-url (gfm-pretty-links--link-url record)
           'gfm-pretty-links-title-attr (gfm-pretty-links--link-title-attr record)
           'gfm-pretty-links-ref-label (gfm-pretty-links--link-ref-label record)
           'gfm-pretty-links-ref-def-pos (gfm-pretty-links--link-ref-def-pos record)
           'display display
           'evaporate t
           (when (eq side 'title)
             (list 'keymap gfm-pretty-links--overlay-keymap)))))

(defun gfm-pretty-links--make-title-overlay (record window id)
  "Create RECORD's title-side overlay for WINDOW with shared id ID."
  (gfm-pretty-links--make-overlay (gfm-pretty-links--link-tbeg record)
                           (gfm-pretty-links--link-tend record)
                           window 'title record id))

(defun gfm-pretty-links--make-url-overlay (record window id)
  "Create RECORD's url-side overlay for WINDOW with shared id ID."
  (gfm-pretty-links--make-overlay (gfm-pretty-links--link-ubeg record)
                           (gfm-pretty-links--link-uend record)
                           window 'url record id))

(defun gfm-pretty-links--decorate-link (record window)
  "Create the title-side and url-side overlays for RECORD in WINDOW.
Degenerate records (empty title span) are skipped."
  (when (< (gfm-pretty-links--link-tbeg record) (gfm-pretty-links--link-tend record))
    (let ((id (gfm-pretty-links--next-id)))
      (gfm-pretty-links--make-title-overlay record window id)
      (when (< (gfm-pretty-links--link-ubeg record) (gfm-pretty-links--link-uend record))
        (gfm-pretty-links--make-url-overlay record window id)))))

;;; Rebuild

(defun gfm-pretty-links--rebuild ()
  "Remove and recreate every gfm-pretty-links overlay in the buffer.
Discovery and overlay creation widen, so the rebuild is
narrowing-resilient: a rebuild within a narrowed region produces the
same overlay set as a widened rebuild.  The reference-definition alist
is recomputed first so reference links resolve against current state."
  (save-restriction
    (widen)
    (save-excursion
      (gfm-pretty-links--remove-overlays)
      (gfm-pretty-links--build-ref-def-alist)
      (let ((blocks (gfm-pretty-links--blocks-in-range (point-min) (point-max)))
            (windows (or (gfm-pretty--display-windows) (list nil))))
        (dolist (window windows)
          (dolist (record blocks)
            (gfm-pretty-links--decorate-link record window)))))))

(defun gfm-pretty-links--schedule-rebuild (&rest _)
  "Arm the debounced rebuild timer after a buffer change."
  (unless (buffer-base-buffer)
    (gfm-pretty--arm-rebuild-timer
     'gfm-pretty-links--rebuild-timer 'gfm-pretty-links-mode #'gfm-pretty-links--rebuild)))

;;; Cursor-driven reveal (per-window, whole-link)

(defun gfm-pretty-links--link-id-at (pos window)
  "Return the `gfm-pretty-links-id' of a revealable overlay covering POS in WINDOW.
Only overlays whose `window' property is nil or WINDOW are considered."
  (cl-loop for ov in (overlays-in pos (min (1+ pos) (point-max)))
           when (and (overlay-get ov 'gfm-pretty-links-revealable)
                     (let ((w (overlay-get ov 'window)))
                       (or (null w) (eq w window))))
           return (overlay-get ov 'gfm-pretty-links-id)))

(defun gfm-pretty-links--restore-overlay (ov)
  "Restore OV's suppressed display property."
  (when (overlay-buffer ov)
    (overlay-put ov 'display (overlay-get ov 'gfm-pretty-links-saved-display))
    (overlay-put ov 'gfm-pretty-links-saved-display nil)))

(defun gfm-pretty-links--reveal ()
  "Reveal the whole link under point in the selected window.
Suppresses `display' on both the title-side and url-side overlays
\(matched by `gfm-pretty-links-id') of the link at point, and restores any
previously-revealed link once point leaves it.  Per-window: overlays
scoped to another window are never touched."
  (let* ((pos (point))
         (win (selected-window))
         (active-id (gfm-pretty-links--link-id-at pos win)))
    (setq gfm-pretty-links--hidden-ovs
          (cl-loop for ov in gfm-pretty-links--hidden-ovs
                   if (and (overlay-buffer ov)
                           active-id
                           (eq (overlay-get ov 'gfm-pretty-links-id) active-id)
                           (let ((w (overlay-get ov 'window)))
                             (or (null w) (eq w win))))
                   collect ov
                   else do (gfm-pretty-links--restore-overlay ov)))
    (when active-id
      (dolist (ov gfm-pretty-links--overlays)
        (when (and (overlay-buffer ov)
                   (eq (overlay-get ov 'gfm-pretty-links-id) active-id)
                   (overlay-get ov 'gfm-pretty-links-revealable)
                   (overlay-get ov 'display)
                   (let ((w (overlay-get ov 'window)))
                     (or (null w) (eq w win)))
                   (not (memq ov gfm-pretty-links--hidden-ovs)))
          (overlay-put ov 'gfm-pretty-links-saved-display (overlay-get ov 'display))
          (overlay-put ov 'display nil)
          (push ov gfm-pretty-links--hidden-ovs))))))

;;; Suppression of the built-in compose path

(define-advice markdown-fontify-inline-links
    (:around (orig &rest args) gfm-pretty-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose branch under `gfm-pretty-links-mode'.
The body still runs — faces apply, properties propagate — only the URL
glyph composition is suppressed, because the gfm-pretty-links overlays
own the link's appearance.  Inert in buffers where the mode is off."
  (if (bound-and-true-p gfm-pretty-links-mode)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

(define-advice markdown-fontify-reference-links
    (:around (orig &rest args) gfm-pretty-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose branch under `gfm-pretty-links-mode'.
See `markdown-fontify-inline-links@gfm-pretty-links-suppress-compose'."
  (if (bound-and-true-p gfm-pretty-links-mode)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

;;; RET / follow-link

(defun gfm-pretty-links--overlay-at-point ()
  "Return a gfm-pretty-links overlay covering point in the selected window, or nil."
  (let ((win (selected-window)))
    (cl-find-if (lambda (ov)
                  (and (overlay-get ov 'gfm-pretty-links-revealable)
                       (let ((w (overlay-get ov 'window)))
                         (or (null w) (eq w win)))))
                (overlays-in (point) (min (1+ (point)) (point-max))))))

(defun gfm-pretty-links-follow-link-at-point ()
  "Follow the decorated link at point via `markdown--browse-url'.
Bound to `RET' through the title-side overlay's `keymap' property, so
off any decorated link `RET' keeps its global binding."
  (interactive)
  (let* ((ov (gfm-pretty-links--overlay-at-point))
         (url (and ov (overlay-get ov 'gfm-pretty-links-url))))
    (if (and url (not (string-empty-p url)))
        (markdown--browse-url url)
      (user-error "Point is not on a decorated link"))))

;;; Reference goto-definition via xref

(defun gfm-pretty-links--xref-backend ()
  "Return the gfm-pretty-links xref backend symbol on a reference link.
Returns nil otherwise so other xref backends are consulted."
  (let ((ov (gfm-pretty-links--overlay-at-point)))
    (and ov
         (overlay-get ov 'gfm-pretty-links-ref-def-pos)
         'gfm-pretty-links)))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gfm-pretty-links)))
  (let ((ov (gfm-pretty-links--overlay-at-point)))
    (and ov (overlay-get ov 'gfm-pretty-links-ref-label))))

(cl-defmethod xref-backend-definitions ((_backend (eql gfm-pretty-links)) identifier)
  (let ((entry (gfm-pretty-links--resolve-ref identifier)))
    (when entry
      (list (xref-make
             identifier
             (xref-make-buffer-location (current-buffer) (nth 2 entry)))))))

;;; Eldoc URL exposure

(defun gfm-pretty-links--eldoc-function (&rest _)
  "Return the resolved URL of the decorated link at point for eldoc.
Includes the inline title attribute when present.  Returns nil off any
decorated link so other eldoc providers are not blocked."
  (let* ((ov (gfm-pretty-links--overlay-at-point))
         (url (and ov (overlay-get ov 'gfm-pretty-links-url)))
         (title (and ov (overlay-get ov 'gfm-pretty-links-title-attr))))
    (when (and url (not (string-empty-p url)))
      (if (and title (not (string-empty-p title)))
          (format "%s — %s" url title)
        url))))

;;; Minor mode

;;;###autoload
(define-minor-mode gfm-pretty-links-mode
  "Decorate Markdown links with per-window overlays.
Replaces the bracket scaffolding of inline, reference, autolink,
bare-URL, and wiki links with a title label and a host-aware icon.
See the file commentary for the full behaviour."
  :lighter " gfm-ln"
  (if gfm-pretty-links-mode
      (progn
        (gfm-pretty-links--rebuild)
        (add-hook 'after-change-functions
                  #'gfm-pretty-links--schedule-rebuild nil t)
        (add-hook 'window-configuration-change-hook
                  #'gfm-pretty-links--schedule-rebuild nil t)
        (add-hook 'post-command-hook #'gfm-pretty-links--reveal nil t)
        (add-hook 'xref-backend-functions #'gfm-pretty-links--xref-backend nil t)
        (add-hook 'eldoc-documentation-functions
                  #'gfm-pretty-links--eldoc-function nil t))
    (remove-hook 'after-change-functions #'gfm-pretty-links--schedule-rebuild t)
    (remove-hook 'window-configuration-change-hook
                 #'gfm-pretty-links--schedule-rebuild t)
    (remove-hook 'post-command-hook #'gfm-pretty-links--reveal t)
    (remove-hook 'xref-backend-functions #'gfm-pretty-links--xref-backend t)
    (remove-hook 'eldoc-documentation-functions
                 #'gfm-pretty-links--eldoc-function t)
    (when (timerp gfm-pretty-links--rebuild-timer)
      (cancel-timer gfm-pretty-links--rebuild-timer))
    (setq gfm-pretty-links--rebuild-timer nil
          gfm-pretty-links--ref-def-alist nil)
    (gfm-pretty-links--remove-overlays)))

;;; markdown-hide-urls integration

(defun gfm-pretty-links--watch-hide-urls (_symbol newval operation where)
  "Track `markdown-hide-urls' changes into `gfm-pretty-links-mode'.
Enabling/disabling stays independent of the variable — toggling the
variable simply follows through to the mode in the affected buffer
\(WHERE, or the current buffer for a global set)."
  (when (eq operation 'set)
    (let ((buf (if (bufferp where) where (current-buffer))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and gfm-pretty-links--watching (derived-mode-p 'markdown-mode))
            (if newval
                (unless gfm-pretty-links-mode (gfm-pretty-links-mode 1))
              (when gfm-pretty-links-mode (gfm-pretty-links-mode -1)))))))))

;;;###autoload
(defun gfm-pretty-links--maybe-enable ()
  "Enable `gfm-pretty-links-mode' when `markdown-hide-urls' is on, and track it.
Wired into `markdown-mode-hook' / `gfm-mode-hook'.  Installs a
buffer-local watcher on `markdown-hide-urls' so later changes to the
variable toggle the mode."
  (setq gfm-pretty-links--watching t)
  (add-variable-watcher 'markdown-hide-urls #'gfm-pretty-links--watch-hide-urls)
  (when (bound-and-true-p markdown-hide-urls)
    (gfm-pretty-links-mode 1)))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty
  (gfm-pretty-define-decorator 'links
    :enable-fn    (lambda () (gfm-pretty-links--maybe-enable))
    :disable-fn   (lambda () (gfm-pretty-links-mode -1))
    :enabled-p-fn (lambda () (bound-and-true-p gfm-pretty-links-mode))))

(provide 'gfm-pretty-links)

;;; gfm-pretty-links.el ends here
