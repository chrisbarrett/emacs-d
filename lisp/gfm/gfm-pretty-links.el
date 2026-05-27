;;; gfm-pretty-links.el --- Overlay decoration for GFM links -*- lexical-binding: t; -*-

;;; Commentary:

;; Minor mode that decorates Markdown / GFM links with per-window
;; overlays, replacing the bracket scaffolding with a clean reading
;; surface:
;;
;;   [Anthropic](https://anthropic.com)   ->   Anthropic <icon>
;;   [Setup](#setup)                      ->   Setup
;;   [ops](./scripts/foo.sh)              ->   ops
;;
;; Each link's resolved URL classifies as `web', `anchor', or `file'.
;; The class drives:
;;
;; - Title-side face: `gfm-pretty-links-title-face' for web,
;;   `gfm-pretty-links-anchor-face' for anchor,
;;   `gfm-pretty-links-file-face' for file.  The two local-link faces
;;   inherit `markdown-link-face' with `:underline nil' so the
;;   underline affordance is reserved for "leaves the buffer".
;; - URL-side overlay: for `web' and `file' links, a single nerd-icons
;;   glyph resolved from the host (web) or filename basename (file).
;;   For `anchor' links, an empty-display overlay that hides the
;;   `(#slug)' span.  Either way the overlay's metadata stays
;;   addressable by RET, eldoc, and xref.  When `nerd-icons' is
;;   unavailable, `web' falls back to a raw URL and `file' falls back
;;   to empty `display' (path stays hidden, no icon).
;; - RET behaviour: anchors jump to the matching heading in-buffer,
;;   file paths open via `find-file' relative to the buffer's
;;   directory, web URLs go through `markdown--browse-url'.
;;
;; Eldoc surfaces the formatted source of the link under point —
;; `[title](url)' (or `[title][label]' for reference links) with
;; `shadow' on scaffolding, the per-class face on the title, and
;; `markdown-url-face' on the URL — so the user can read the target
;; without disturbing rendering.
;;
;; Overlays are per-window (via the `window' overlay property) and
;; carry the resolved metadata as overlay properties so RET, eldoc,
;; and the xref backend read them without re-parsing.
;;
;; markdown-mode's built-in `markdown-hide-urls' compose-region URL
;; collapse is suppressed while the mode is on, via `:around' advice on
;; `markdown-fontify-inline-links' / `markdown-fontify-reference-links'
;; gated by the buffer-local mode variable.
;;
;; `M-.' on a reference link jumps to the `[label]:' definition line
;; via an `xref' backend.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'xref)
(require 'eldoc)
(require 'markdown-mode)
(require 'gfm-pretty-engine)
(require 'nerd-icons nil t)

(defvar gfm-pretty-mode)
(declare-function gfm-pretty-mode "gfm-pretty")
(declare-function gfm-pretty--require-all "gfm-pretty")
(declare-function gfm-pretty-toggle-decorator "gfm-pretty")

(defgroup gfm-pretty-links nil
  "Overlay decoration for GitHub Flavored Markdown links."
  :group 'markdown-faces)

(defcustom gfm-pretty-links-title-face 'markdown-link-face
  "Face used for the display text of a decorated `web' link's title side."
  :type 'face
  :group 'gfm-pretty-links)

(defface gfm-pretty-links-anchor-face
  '((t :inherit markdown-link-face :underline nil))
  "Title-side face for links whose target is a buffer-internal anchor."
  :group 'gfm-pretty-links)

(defface gfm-pretty-links-file-face
  '((t :inherit markdown-link-face :underline nil))
  "Title-side face for links whose target is a relative or absolute path."
  :group 'gfm-pretty-links)

(defvar-keymap gfm-pretty-links--overlay-keymap
  :doc "Keymap active inside a decorated link's title-side overlay."
  "RET" #'gfm-pretty-links-follow-link-at-point)

(defvar gfm-pretty-links-after-anchor-jump-functions nil
  "Abnormal hook run after a successful anchor-link jump.
Each function receives one argument: the target buffer position (start
of the matched heading line, in widened coordinates).  The hook fires
after `gfm-pretty-links--jump-to-anchor' has widened the buffer and
moved point.  Subscribers can use it to restore their preferred
narrowing or apply additional decoration.  Not run on a miss.")

;;; Buffer-local state

(defvar-local gfm-pretty-links--ref-def-alist nil
  "Buffer-local alist mapping a downcased reference label to its definition.
Each value is (URL TITLE-ATTR DEF-POS): the resolved URL string, the
optional title attribute, and the buffer position of the `[label]:'
definition line.  Recomputed at the start of every rebuild; the first
definition for a duplicate label wins.")

(defvar-local gfm-pretty-links--watching nil
  "Non-nil in buffers that opted into `markdown-hide-urls' tracking.
Set by `gfm-pretty-links--maybe-enable'.  The global `markdown-hide-urls'
variable watcher only toggles the mode in buffers carrying this flag,
which keeps the watcher effectively buffer-local: a plain
`markdown-mode' buffer that never ran `gfm-pretty-links--maybe-enable' is
left alone.")

(defconst gfm-pretty-links--registry
  (gfm-pretty--registry-for 'links 'gfm-pretty-links)
  "Shared overlay-registry context for gfm-pretty-links.")

(defsubst gfm-pretty-links--remove-overlays (&optional beg end)
  "Remove all gfm-pretty-links overlays between BEG and END (full reset when nil)."
  (gfm-pretty--remove-overlays gfm-pretty-links--registry beg end))

;;; URL classification

(defconst gfm-pretty-links--file-prefix-re
  (rx bos (or "./" "../" "/" "file:"))
  "URL prefix that classifies a link target as `file'.")

(defun gfm-pretty-links--classify-url (url)
  "Return the target class of URL: one of `web', `anchor', `file'.
Pure function of the URL string.  `#…' is anchor; `./', `../', `/',
`file:' is file; everything else (any scheme, host-relative URL,
empty / nil) is web."
  (cond
   ((or (null url) (string-empty-p url)) 'web)
   ((eq (aref url 0) ?#) 'anchor)
   ((string-match-p gfm-pretty-links--file-prefix-re url) 'file)
   (t 'web)))

(defun gfm-pretty-links--strip-wrapping-backticks (s)
  "Return S without a single pair of wrapping backticks, when present.
A pair is \"wrapping\" only when S begins and ends with `, and the
inner substring has no `, so labels like `foo` strip but `say `hi`
world' is left untouched."
  (if (and (>= (length s) 2)
           (eq (aref s 0) ?`)
           (eq (aref s (1- (length s))) ?`)
           (not (string-search "`" (substring s 1 -1))))
      (substring s 1 -1)
    s))

(defun gfm-pretty-links--title-face-for-class (class)
  "Return the title-side face for link CLASS."
  (pcase class
    ('anchor 'gfm-pretty-links-anchor-face)
    ('file   'gfm-pretty-links-file-face)
    (_       gfm-pretty-links-title-face)))

;;; URL-form deferral to `link-previews'

(defconst gfm-pretty-links--source-range-url-rx
  (rx bos
      (+ (not (any "#")))           ; path
      "#L" (+ digit)
      (? "-L" (+ digit))
      eos)
  "Regexp matching `<path>#L<n>[-L<n>]' source-range URLs.")

(defconst gfm-pretty-links--diff-url-rx
  (rx bos "diff:"
      (+ nonl) "..."                ; base...
      (+ (not (any "#")))           ; head (no `#')
      (? "#" (* nonl))              ; optional file scope
      eos)
  "Regexp matching `diff:<base>...<head>[#<path>]' diff URLs.")

(defun gfm-pretty-links--source-range-url-p (url)
  "Non-nil when URL matches the source-range `<path>#L<n>[-L<n>]' shape."
  (and url (string-match-p gfm-pretty-links--source-range-url-rx url)))

(defun gfm-pretty-links--diff-url-p (url)
  "Non-nil when URL matches the diff `diff:<base>...<head>[#<path>]' shape."
  (and url (string-match-p gfm-pretty-links--diff-url-rx url)))

(defun gfm-pretty-links--skip-record-p (record)
  "Non-nil when RECORD should be deferred to the `link-previews' decorator.
Deferral matches `link-previews'' own claim rules so spans that
neither decorator would render do not silently disappear:

- Diff-URL records (`diff:<base>...<head>[#<path>]') are deferred
  unconditionally.
- Inline source-range records (`[label](<path>#L<n>[-L<n>])')
  whose `[label](url)' span is standalone on its line (per
  `gfm-pretty-standalone-span-p') are deferred.  Reference,
  shortcut, autolink, wiki, and inline-in-prose source-range
  records fall through to normal decoration."
  (let ((url (gfm-pretty-links--link-url record)))
    (cond
     ((gfm-pretty-links--diff-url-p url) t)
     ((and (eq (gfm-pretty-links--link-kind record) 'inline)
           (gfm-pretty-links--source-range-url-p url))
      (let ((span (gfm-pretty-links--record-span record)))
        (gfm-pretty-standalone-span-p (car span) (cdr span))))
     (t nil))))

;;; Link records

(cl-defstruct (gfm-pretty-links--link
               (:constructor gfm-pretty-links--make-link)
               (:copier nil))
  "One decorated link.
KIND is one of `inline', `reference', `autolink', `bare-url', `wiki'.
CLASS is one of `web', `anchor', `file' — the resolved-URL target class.
TBEG/TEND bound the title-side overlay; LABEL is its display string.
UBEG/UEND bound the url-side overlay.  URL is the resolved target.
TITLE-ATTR is the inline title attribute, when present.  REF-LABEL is
the reference label (reference links only); REF-DEF-POS is the buffer
position of its `[label]:' definition line."
  kind class tbeg tend label ubeg uend url title-attr ref-label ref-def-pos)

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

(defun gfm-pretty-links--strip-url-fragment (url)
  "Return URL with any `#…' fragment removed.
A `#'-prefixed URL (same-document anchor) is returned unchanged so
the caller's anchor branch keeps the slug."
  (cond
   ((or (null url) (string-empty-p url)) url)
   ((eq (aref url 0) ?#) url)
   (t (let ((hash (string-match-p "#" url)))
        (if hash (substring url 0 hash) url)))))

(defun gfm-pretty-links--icon-for-target (url)
  "Return a single nerd-icons glyph for URL, deferring entirely to nerd-icons.
http(s) URLs, same-document anchors, and other absolute schemes resolve
via `nerd-icons-icon-for-url'; relative paths and `file:' URLs resolve
via `nerd-icons-icon-for-file' on the basename, with any trailing
`#…' fragment stripped first so source-range URLs resolve their
underlying file extension.  No `:height' override is passed.
Returns nil when nerd-icons is unavailable."
  (when (and url (not (string-empty-p url)))
    (cond
     ((string-match-p (rx bos (or "http://" "https://")) url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "#" url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     ((string-prefix-p "file:" url)
      (gfm-pretty-links--call-nerd
       #'nerd-icons-icon-for-file
       (file-name-nondirectory
        (gfm-pretty-links--strip-url-fragment
         (string-remove-prefix "file:" url)))))
     ((string-match-p (rx bos (+ (any "a-zA-Z0-9.+-")) ":") url)
      (gfm-pretty-links--call-nerd #'nerd-icons-icon-for-url url))
     (t
      (gfm-pretty-links--call-nerd
       #'nerd-icons-icon-for-file
       (file-name-nondirectory
        (gfm-pretty-links--strip-url-fragment url)))))))

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

(defun gfm-pretty-links--in-code-p (pos)
  "Non-nil if POS lies inside a markdown code region.
Delegates to `markdown-code-block-at-pos' (fenced + indented +
pre) and `markdown-inline-code-at-pos-p' (inline spans).  Used to
keep the links decorator out of code where `[foo](bar)' syntax is
just text."
  (or (markdown-code-block-at-pos pos)
      (markdown-inline-code-at-pos-p pos)))

(defun gfm-pretty-links--scan-inline (beg end)
  "Return inline-link records found between BEG and END.
Image links (`![alt](url)') are rejected via the leading-bang group."
  (let (records)
    (save-excursion
      (save-match-data
        (goto-char beg)
        (while (re-search-forward markdown-regex-link-inline end t)
          (unless (match-beginning 1)   ; leading `!' => image, skip
            (let* ((label (match-string-no-properties 3))
                   (url (string-trim (or (match-string-no-properties 6) "")))
                   (title (gfm-pretty-links--strip-title-quotes
                           (match-string-no-properties 7)))
                   (tbeg (match-beginning 2)) (tend (match-end 4))
                   (ubeg (match-beginning 5)) (uend (match-end 8))
                   (class (gfm-pretty-links--classify-url url)))
              (push (gfm-pretty-links--make-link
                     :kind 'inline
                     :class class
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
                (let ((url (nth 0 entry)))
                  (push (gfm-pretty-links--make-link
                         :kind 'reference
                         :class (gfm-pretty-links--classify-url url)
                         :tbeg (match-beginning 2) :tend (match-end 4)
                         :label (if (string-empty-p text) ref-label text)
                         :ubeg (match-beginning 5) :uend (match-end 7)
                         :url url
                         :title-attr (nth 1 entry)
                         :ref-label ref-label
                         :ref-def-pos (nth 2 entry))
                        records))))))))
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
                  (let ((url (nth 0 entry)))
                    (push (gfm-pretty-links--make-link
                           :kind 'reference
                           :class (gfm-pretty-links--classify-url url)
                           :tbeg mbeg :tend mend
                           :label label
                           ;; No second bracket pair: hang the icon off
                           ;; the closing `]'.
                           :ubeg (1- mend) :uend mend
                           :url url
                           :title-attr (nth 1 entry)
                           :ref-label label
                           :ref-def-pos (nth 2 entry))
                          records)))))))))
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
     :class (gfm-pretty-links--classify-url url)
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
                     :class (gfm-pretty-links--classify-url target)
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
                    (gfm-pretty-links--in-code-p (car span))
                    (gfm-pretty-links--skip-record-p record)
                    (cl-some (lambda (c)
                               (gfm-pretty--region-overlaps-p span c))
                             claimed))
          (push span claimed)
          (push record kept))))
    (sort kept (lambda (a b)
                 (< (gfm-pretty-links--link-tbeg a) (gfm-pretty-links--link-tbeg b))))))

;;; Overlay construction

(defun gfm-pretty-links--make-overlay (beg end window side record)
  "Create one gfm-pretty-links overlay over [BEG, END) for WINDOW.
SIDE is `title' or `url'.  RECORD is the `gfm-pretty-links--link' it
belongs to.  The overlay carries the link's resolved metadata so RET,
eldoc, and the xref backend can read it without re-parsing."
  (let* ((class (gfm-pretty-links--link-class record))
         (display
          (cond
           ((eq side 'title)
            (propertize (gfm-pretty-links--strip-wrapping-backticks
                         (gfm-pretty-links--link-label record))
                        'face (gfm-pretty-links--title-face-for-class class)))
           ((eq class 'anchor) "")
           (t
            (or (gfm-pretty-links--icon-for-target (gfm-pretty-links--link-url record))
                "")))))
    (apply #'gfm-pretty--make-display
           gfm-pretty-links--registry beg end window
           'gfm-pretty-links-class class
           'gfm-pretty-links-side side
           'gfm-pretty-links-kind (gfm-pretty-links--link-kind record)
           'gfm-pretty-links-url (gfm-pretty-links--link-url record)
           'gfm-pretty-links-label (gfm-pretty-links--link-label record)
           'gfm-pretty-links-title-attr (gfm-pretty-links--link-title-attr record)
           'gfm-pretty-links-ref-label (gfm-pretty-links--link-ref-label record)
           'gfm-pretty-links-ref-def-pos (gfm-pretty-links--link-ref-def-pos record)
           'display display
           'evaporate t
           (when (eq side 'title)
             (list 'keymap gfm-pretty-links--overlay-keymap)))))

(defun gfm-pretty-links--make-title-overlay (record window)
  "Create RECORD's title-side overlay for WINDOW."
  (gfm-pretty-links--make-overlay (gfm-pretty-links--link-tbeg record)
                           (gfm-pretty-links--link-tend record)
                           window 'title record))

(defun gfm-pretty-links--make-url-overlay (record window)
  "Create RECORD's url-side overlay for WINDOW."
  (gfm-pretty-links--make-overlay (gfm-pretty-links--link-ubeg record)
                           (gfm-pretty-links--link-uend record)
                           window 'url record))

(defun gfm-pretty-links--decorate-link (record window)
  "Create RECORD's per-side overlays in WINDOW.
Degenerate records (empty title span) are skipped.  Web and file
links get a url-side icon overlay (nerd-icons by host or basename);
anchor links get a url-side overlay whose `display' is empty, hiding
the `(#slug)' span.  Either way the overlay's metadata stays
addressable for RET dispatch, eldoc, and xref.  When `nerd-icons' is
unavailable, file links fall back to empty `display'."
  (when (< (gfm-pretty-links--link-tbeg record) (gfm-pretty-links--link-tend record))
    (gfm-pretty-links--make-title-overlay record window)
    (when (and (memq (gfm-pretty-links--link-class record) '(web anchor file))
               (< (gfm-pretty-links--link-ubeg record) (gfm-pretty-links--link-uend record)))
      (gfm-pretty-links--make-url-overlay record window))))

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

(defun gfm-pretty-links--collect-blocks ()
  "Return link records widened over the whole buffer.
Engine entry point — the engine's reconciler reads ranges from these."
  (save-restriction
    (widen)
    (gfm-pretty-links--build-ref-def-alist)
    (gfm-pretty-links--blocks-in-range (point-min) (point-max))))

(defun gfm-pretty-links--block-range (record)
  "Engine `:range-fn' — return RECORD's full span as (BEG . END)."
  (gfm-pretty-links--record-span record))

(defun gfm-pretty-links--apply-block (record window)
  "Engine `:apply-block-fn' — decorate RECORD in WINDOW."
  (save-restriction
    (widen)
    (gfm-pretty-links--decorate-link record window)))

;;; Suppression of the built-in compose path

(define-advice markdown-fontify-inline-links
    (:around (orig &rest args) gfm-pretty-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose branch under the links decorator.
The body still runs — faces apply, properties propagate — only the URL
glyph composition is suppressed, because the gfm-pretty-links overlays
own the link's appearance.  Inert in buffers where the mode is off."
  (if (gfm-pretty--state-get 'links 'enabled-p)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

(define-advice markdown-fontify-reference-links
    (:around (orig &rest args) gfm-pretty-links-suppress-compose)
  "Skip the `markdown-hide-urls' compose branch under the links decorator.
See `markdown-fontify-inline-links@gfm-pretty-links-suppress-compose'."
  (if (gfm-pretty--state-get 'links 'enabled-p)
      (let ((markdown-hide-urls nil)) (apply orig args))
    (apply orig args)))

;;; Overlay lookup at point

(defun gfm-pretty-links--overlay-at-point ()
  "Return a gfm-pretty-links overlay covering point in the selected window, or nil.
Marker prop is `gfm-pretty-links-class' — every overlay this decorator
creates carries it, on both title and url sides."
  (let ((win (selected-window)))
    (cl-find-if (lambda (ov)
                  (and (overlay-get ov 'gfm-pretty-links-class)
                       (let ((w (overlay-get ov 'window)))
                         (or (null w) (eq w win)))))
                (overlays-in (point) (min (1+ (point)) (point-max))))))

;;; RET / follow-link

(defun gfm-pretty-links--heading-slug (text)
  "Return a GitHub-flavoured anchor slug for heading TEXT.
Lower-case; ASCII alphanumerics, hyphens, and underscores are kept;
runs of whitespace fold to a single hyphen; everything else drops."
  (let* ((s (downcase (string-trim text)))
         (s (replace-regexp-in-string "[^[:alnum:][:space:]_-]" "" s))
         (s (replace-regexp-in-string "[[:space:]]+" "-" s)))
    s))

(defun gfm-pretty-links--jump-to-anchor (anchor)
  "Move point to the heading whose generated slug matches ANCHOR (no `#').
Walks atx and setext headings across the widened buffer so anchors
resolve regardless of current narrowing.  On a successful match,
records the click site via `gfm-pretty-links--record-jump' (so
better-jumper / evil's jump list captures it), pushes it onto the
mark ring, widens the buffer, moves point to the heading, then runs
`gfm-pretty-links-after-anchor-jump-functions' with the target buffer
position.  Signals `user-error' when no matching heading is found and
does not run the hook."
  (let ((slug (string-remove-prefix "#" anchor))
        (start (point))
        (found nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (save-match-data
          (while (and (not found)
                      (re-search-forward markdown-regex-header nil t))
            (let* ((text (or (match-string-no-properties 5)
                             (match-string-no-properties 1))))
              (when (and text
                         (equal slug (gfm-pretty-links--heading-slug text)))
                (setq found (match-beginning 0))))))))
    (if found
        (progn (gfm-pretty-links--record-jump start)
               (push-mark start)
               (widen)
               (goto-char found)
               (run-hook-with-args
                'gfm-pretty-links-after-anchor-jump-functions found))
      (user-error "No heading matches anchor: #%s" slug))))

(defun gfm-pretty-links--record-jump (pos)
  "Record POS in the active jump-list implementation, if any.
Prefers `better-jumper-set-jump' (used by this config), falls back to
`evil-set-jump'.  Silently noops when neither is available."
  (cond
   ((fboundp 'better-jumper-set-jump) (better-jumper-set-jump pos))
   ((fboundp 'evil-set-jump) (evil-set-jump pos))))

(defconst gfm-pretty-links--source-range-fragment-rx
  (rx "#L" (group (+ digit)) (? "-L" (+ digit)) eos)
  "Regexp matching an `#L<n>[-L<n>]' source-range fragment.
Group 1 captures the start line number.")

(defun gfm-pretty-links--follow-file (url)
  "Open file at URL via `find-file', expanded against the buffer's directory.
URL is a path beginning with `./', `../', `/', or a `file:' URI.
When URL carries an `#L<n>[-L<n>]' source-range fragment, point jumps
to line <n> after the file opens; the range-end is ignored."
  (let* ((stripped (if (string-prefix-p "file:" url)
                       (string-remove-prefix "file:" url)
                     url))
         (line (and (string-match
                     gfm-pretty-links--source-range-fragment-rx
                     stripped)
                    (string-to-number (match-string 1 stripped))))
         (path (gfm-pretty-links--strip-url-fragment stripped))
         (base (or (and buffer-file-name
                        (file-name-directory buffer-file-name))
                   default-directory)))
    (find-file (expand-file-name path base))
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun gfm-pretty-links-follow-link-at-point ()
  "Follow the decorated link at point, dispatched by target class.
Web URLs go through `markdown--browse-url'; anchor links jump to the
heading whose generated slug matches; file links open via `find-file'
relative to the buffer's directory.  Bound to `RET' through the
title-side overlay's `keymap' property, so off any decorated link
`RET' keeps its global binding."
  (interactive)
  (let* ((ov (gfm-pretty-links--overlay-at-point))
         (url (and ov (overlay-get ov 'gfm-pretty-links-url)))
         (class (and ov (overlay-get ov 'gfm-pretty-links-class))))
    (unless (and url (not (string-empty-p url)))
      (user-error "Point is not on a decorated link"))
    (pcase class
      ('anchor (gfm-pretty-links--jump-to-anchor url))
      ('file   (gfm-pretty-links--follow-file url))
      (_       (markdown--browse-url url)))))

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

;;; Eldoc: formatted-source surface

(defun gfm-pretty-links--shadow (s)
  "Return scaffolding string S faced with `shadow'."
  (propertize s 'face 'shadow))

(defun gfm-pretty-links--titled (text class)
  "Return TEXT faced with the title face for CLASS."
  (propertize text 'face (gfm-pretty-links--title-face-for-class class)))

(defun gfm-pretty-links--urlised (url)
  "Return URL faced with `markdown-url-face'."
  (propertize url 'face 'markdown-url-face))

(defun gfm-pretty-links--title-suffix (title-attr)
  "Return the eldoc tail for a non-empty TITLE-ATTR, or empty string."
  (if (and title-attr (not (string-empty-p title-attr)))
      (concat (gfm-pretty-links--shadow " — ")
              (propertize (format "\"%s\"" title-attr) 'face 'italic))
    ""))

(defun gfm-pretty-links--eldoc-format (kind class label url title-attr ref-label)
  "Return the propertised eldoc string for a link with the given fields.
KIND is the link kind; CLASS its target class; LABEL the displayed
title; URL the resolved target; TITLE-ATTR an optional inline title
attribute; REF-LABEL the reference label (reference kind only)."
  (pcase kind
    ('inline
     (concat (gfm-pretty-links--shadow "[")
             (gfm-pretty-links--titled label class)
             (gfm-pretty-links--shadow "](")
             (gfm-pretty-links--urlised url)
             (gfm-pretty-links--shadow ")")
             (gfm-pretty-links--title-suffix title-attr)))
    ('reference
     (if (and ref-label (string= label ref-label))
         ;; Shortcut (or `[name]==[label]') reference.
         (concat (gfm-pretty-links--shadow "[")
                 (gfm-pretty-links--titled label class)
                 (gfm-pretty-links--shadow "]"))
       (concat (gfm-pretty-links--shadow "[")
               (gfm-pretty-links--titled label class)
               (gfm-pretty-links--shadow "][")
               (gfm-pretty-links--titled (or ref-label "") class)
               (gfm-pretty-links--shadow "]"))))
    ('autolink
     (concat (gfm-pretty-links--shadow "<")
             (gfm-pretty-links--urlised url)
             (gfm-pretty-links--shadow ">")))
    ('bare-url
     (gfm-pretty-links--urlised url))
    ('wiki
     (concat (gfm-pretty-links--shadow "[[")
             (gfm-pretty-links--titled label class)
             (gfm-pretty-links--shadow "]]")))
    (_ url)))

(defun gfm-pretty-links--eldoc-function (&rest _)
  "Return the propertised source of the decorated link at point for eldoc.
Returns nil off any decorated link so other eldoc providers are not blocked."
  (let ((ov (gfm-pretty-links--overlay-at-point)))
    (when ov
      (let ((kind (overlay-get ov 'gfm-pretty-links-kind))
            (class (overlay-get ov 'gfm-pretty-links-class))
            (label (overlay-get ov 'gfm-pretty-links-label))
            (url (overlay-get ov 'gfm-pretty-links-url))
            (title-attr (overlay-get ov 'gfm-pretty-links-title-attr))
            (ref-label (overlay-get ov 'gfm-pretty-links-ref-label)))
        (when (and kind url (not (string-empty-p url)))
          (gfm-pretty-links--eldoc-format kind class label url title-attr ref-label))))))

;;; Lifecycle hooks delegated to engine

(defun gfm-pretty-links--on-enable ()
  "Install link-decorator-specific hooks (xref + eldoc)."
  (add-hook 'xref-backend-functions #'gfm-pretty-links--xref-backend nil t)
  (add-hook 'eldoc-documentation-functions
            #'gfm-pretty-links--eldoc-function nil t))

(defun gfm-pretty-links--on-disable ()
  "Remove the link-decorator-specific hooks and reset reference cache."
  (remove-hook 'xref-backend-functions #'gfm-pretty-links--xref-backend t)
  (remove-hook 'eldoc-documentation-functions
               #'gfm-pretty-links--eldoc-function t)
  (setq gfm-pretty-links--ref-def-alist nil))

;;; markdown-hide-urls integration

(defun gfm-pretty-links--enabled-p ()
  "Non-nil when the links decorator is on in the current buffer."
  (gfm-pretty--state-get 'links 'enabled-p))

(defun gfm-pretty-links--watch-hide-urls (_symbol newval operation where)
  "Track `markdown-hide-urls' changes into the links decorator.
Enabling/disabling stays independent of the variable — toggling the
variable follows through to the links decorator in WHERE (or the
current buffer for a global set) via `gfm-pretty-mode' +
`gfm-pretty-toggle-decorator'."
  (when (eq operation 'set)
    (let ((buf (if (bufferp where) where (current-buffer))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and gfm-pretty-links--watching (derived-mode-p 'markdown-mode))
            (cond
             ;; Enable: ensure umbrella is on, then ensure links bit is on.
             (newval
              (unless gfm-pretty-mode (gfm-pretty-mode 1))
              (unless (gfm-pretty-links--enabled-p)
                (gfm-pretty-toggle-decorator 'links)))
             ;; Disable: only flip the links bit; leave umbrella alone.
             (t
              (when (gfm-pretty-links--enabled-p)
                (gfm-pretty-toggle-decorator 'links))))))))))

;;;###autoload
(defun gfm-pretty-links--maybe-enable ()
  "Enable the links decorator when `markdown-hide-urls' is on, and track it.
Wired into `markdown-mode-hook' / `gfm-mode-hook'.  Installs a
variable watcher on `markdown-hide-urls' so later changes flip the
decorator's enable bit via `gfm-pretty-toggle-decorator'."
  (setq gfm-pretty-links--watching t)
  (add-variable-watcher 'markdown-hide-urls #'gfm-pretty-links--watch-hide-urls)
  (when (bound-and-true-p markdown-hide-urls)
    (unless gfm-pretty-mode (gfm-pretty-mode 1))
    (unless (gfm-pretty-links--enabled-p)
      (gfm-pretty-toggle-decorator 'links))))

;;; gfm-pretty decorator registration

(with-eval-after-load 'gfm-pretty-engine
  (gfm-pretty-define-decorator 'links
    :registry           gfm-pretty-links--registry
    :collect-fn         #'gfm-pretty-links--collect-blocks
    :range-fn           #'gfm-pretty-links--block-range
    :apply-block-fn     #'gfm-pretty-links--apply-block
    :rebuild-fn         #'gfm-pretty-links--rebuild
    :on-enable-fn       #'gfm-pretty-links--on-enable
    :on-disable-fn      #'gfm-pretty-links--on-disable))

(provide 'gfm-pretty-links)

;;; gfm-pretty-links.el ends here
