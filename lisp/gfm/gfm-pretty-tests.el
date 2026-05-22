;;; gfm-pretty-tests.el --- Tests for gfm-pretty visual decoration -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the GFM pretty visual-decoration library family.

;;; Code:

(require 'ert)
(require 'gfm-pretty)
(require 'gfm-pretty-borders)
(require 'gfm-pretty-engine)
(require 'gfm-pretty-callouts)
(require 'gfm-pretty-fences)
(require 'gfm-pretty-tables)
(require 'gfm-pretty-hrule)
(require 'gfm-pretty-links)

;; Load lang-markdown composition (config of `markdown-code-lang-modes',
;; `gfm-mode-hook' wiring, `major-mode-remap-alist') so tests that exercise
;; behaviour driven by that config see the live values.
(let* ((root (file-name-as-directory
              (expand-file-name "../.." (file-name-directory
                                         (or load-file-name
                                             buffer-file-name)))))
       (init (expand-file-name "modules/lang-markdown/init.el" root))
       (lib (expand-file-name "modules/lang-markdown/lib.el" root)))
  (condition-case nil
      (progn
        (when (file-exists-p lib) (load lib nil 'nomessage))
        (when (file-exists-p init) (load init nil 'nomessage)))
    (error nil)))

;;; gfm-pretty-borders unit tests

(ert-deftest lang-markdown/gfm-pretty--simulate-wrap-zero-width-terminates ()
  "`gfm-pretty--simulate-wrap' returns rather than spinning at width 0."
  (let ((res (with-timeout (1 'timeout)
               (gfm-pretty--simulate-wrap "hello world" 0))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-pretty--simulate-wrap-tiny-width-with-prefix-terminates ()
  "`gfm-pretty--simulate-wrap' terminates when width ≤ cont-prefix-w."
  (let ((res (with-timeout (1 'timeout)
               (gfm-pretty--simulate-wrap "hello world" 1 2))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-pretty--simulate-wrap-no-wrap-fits ()
  "Text fitting in WIDTH does not produce wrap positions."
  (let ((res (gfm-pretty--simulate-wrap "hello" 80)))
    (should (= 5 (car res)))
    (should-not (cdr res))))

(ert-deftest lang-markdown/gfm-pretty--simulate-wrap-breaks-at-space ()
  "Wrap chooses the last space within the line slice."
  (let ((res (gfm-pretty--simulate-wrap "hello world foo" 8)))
    ;; First line takes "hello wo" -> wrap at last space (after "hello ").
    (should (consp (cdr res)))))

(ert-deftest lang-markdown/gfm-pretty--max-line-width-respects-indent ()
  "`gfm-pretty--max-line-width' subtracts INDENT per line."
  (with-temp-buffer
    (insert "    aaaa\n    aa\n    aaaaaa\n")
    (should (= 6 (gfm-pretty--max-line-width
                  (point-min) (point-max) 4)))
    (should (= 10 (gfm-pretty--max-line-width
                   (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty--region-overlaps-p ()
  (should (gfm-pretty--region-overlaps-p '(1 . 5) '(3 . 7)))
  (should (gfm-pretty--region-overlaps-p '(1 . 5) '(5 . 9)))
  (should-not (gfm-pretty--region-overlaps-p '(1 . 4) '(5 . 9))))

(ert-deftest lang-markdown/gfm-pretty--in-ranges-p ()
  (should (gfm-pretty--in-ranges-p 5 '((1 . 4) (5 . 9))))
  (should-not (gfm-pretty--in-ranges-p 12 '((1 . 4) (5 . 9)))))

(ert-deftest lang-markdown/gfm-pretty--normalised-border-face-resets-styling ()
  "Normalised face spec resets slant/weight/underline/etc.
`:weight light' to draw a hairline box; explicit so an inherited
`:weight bold' (e.g. via font-lock) cannot leak through."
  (let ((spec (gfm-pretty--normalised-border-face 'italic)))
    (should (equal (plist-get spec :inherit) 'italic))
    (should (eq (plist-get spec :slant) 'normal))
    (should (eq (plist-get spec :weight) 'light))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))))

(ert-deftest lang-markdown/gfm-pretty--wrap-prefix-uses-hook-arrow ()
  "Continuation glyph is `↪'."
  (let ((s (gfm-pretty--wrap-prefix 'default)))
    (should (string-match-p "↪" s))
    (should-not (string-match-p "⋱" s))))

(ert-deftest lang-markdown/gfm-pretty--right-after-tail-fills-to-window-edge ()
  "`right-after' ends with `(space :align-to right)' in the default face.
Regression: an overlay face with `:extend nil' does NOT clip a
sibling `:extend t' face's past-EOL fill — Emacs's
`face_at_buffer_position' merge skips opted-out faces rather than
suppressing opted-in ones.  The working idiom is to physically
fill the visual line to the window edge with the default
background, leaving no past-EOL region for `:extend' to colour."
  (let* ((after (gfm-pretty--right-after 40 'default))
         (tail-i (1- (length after))))
    (should (equal '(space :align-to right)
                   (get-text-property tail-i 'display after)))
    (should (eq 'default (get-text-property tail-i 'face after)))))

(ert-deftest lang-markdown/gfm-pretty--right-after-overflow-tail-fills-to-window-edge ()
  "`right-after-overflow' ends with `(space :align-to right)' in the default face."
  (let* ((after (gfm-pretty--right-after-overflow
                 'default "x" nil))
         (tail-i (1- (length after))))
    (should (equal '(space :align-to right)
                   (get-text-property tail-i 'display after)))
    (should (eq 'default (get-text-property tail-i 'face after)))))

;;; Narrowing-resilient shared teardown

(ert-deftest lang-markdown/gfm-pretty--remove-overlays-full-clear-widens ()
  "`--remove-overlays' with no BEG/END clears overlays outside the narrowing.
Regression: under `gfm-present-mode' the buffer is narrowed when a full
rebuild fires; the prior implementation left tagged overlays sitting
outside the restriction (zombies on widen).  See the
fix-gfm-narrowing-safety change."
  :tags '(narrowing-regression)
  (let ((overlays nil))
    (let* ((name 'lm-test-decorator)
           (registry (gfm-pretty--registry-for name 'lm-test-tag)))
      (with-temp-buffer
        (gfm-pretty--state-set name 'overlays nil)
        (insert "first region\n\n--- divider ---\n\nsecond region\n")
        (let ((ov-a (make-overlay 1 5))
              (ov-b (make-overlay 30 35)))
          (overlay-put ov-a 'lm-test-tag t)
          (overlay-put ov-b 'lm-test-tag t)
          (gfm-pretty--state-set name 'overlays (list ov-a ov-b)))
        (narrow-to-region 1 10)
        (gfm-pretty--remove-overlays registry)
        (widen)
        (setq overlays
              (cl-remove-if-not
               (lambda (ov) (overlay-get ov 'lm-test-tag))
               (overlays-in (point-min) (point-max))))))
    (should-not overlays)))

;;; gfm-pretty-callouts tests

(require 'gfm-pretty-callouts)

(defun lang-markdown-tests--callout-block (type body)
  "Build a callout source block of TYPE with BODY lines."
  (concat (format "> [!%s]\n" type)
          (mapconcat (lambda (l) (concat "> " l)) body "\n")
          "\n"))

(ert-deftest lang-markdown/gfm-pretty-callouts-find-blocks-detects-types ()
  "Each known callout type is detected with its label."
  (dolist (type '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
    (with-temp-buffer
      (insert (lang-markdown-tests--callout-block type '("hello")))
      (let ((blocks (gfm-pretty-callouts--find-blocks)))
        (should (= 1 (length blocks)))
        (should (equal type (nth 2 (car blocks))))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-find-blocks-multiline ()
  "Block end extends through subsequent blockquote lines."
  (with-temp-buffer
    (insert "> [!IMPORTANT]\n> line one\n> line two\n\nplain\n")
    (let* ((blocks (gfm-pretty-callouts--find-blocks))
           (block (car blocks)))
      (should (= 1 (length blocks)))
      (goto-char (nth 1 block))
      (should (string= "> line two"
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-find-blocks-ignores-plain-blockquote ()
  (with-temp-buffer
    (insert "> just a quote\n> with two lines\n")
    (should-not (gfm-pretty-callouts--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-callouts-mode-creates-overlays ()
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-callouts))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-mode-removes-overlays ()
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (gfm-pretty-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-callouts))
                         (overlays-in (point-min) (point-max))))))

(defun lang-markdown-tests--prefix-overlays ()
  "Return all callout prefix overlays in the current buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'gfm-pretty-callouts-revealable))
   (overlays-in (point-min) (point-max))))

(ert-deftest lang-markdown/gfm-pretty-callouts-prefix-overlays-evaporative ()
  "Marker and body display overlays are evaporative and revealable."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let ((ovs (lang-markdown-tests--prefix-overlays)))
      (should (= 2 (length ovs)))
      (dolist (ov ovs)
        (should (overlay-get ov 'evaporate))
        (should (overlay-get ov 'gfm-pretty-callouts-revealable))
        (should (stringp (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-marker-covers-whole-line ()
  "Marker overlay covers the full marker line; body overlay covers `> '."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let* ((ovs (sort (lang-markdown-tests--prefix-overlays)
                      (lambda (a b) (< (overlay-start a)
                                       (overlay-start b)))))
           (marker (nth 0 ovs))
           (body   (nth 1 ovs)))
      ;; `> [!NOTE]' is 9 chars.
      (should (= 9 (- (overlay-end marker) (overlay-start marker))))
      (should (= 2 (- (overlay-end body)   (overlay-start body)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-prefix-display-marker-vs-body ()
  "Marker displays `┌─ TITLE'; body displays the side edge `│ '."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let* ((ovs (sort (lang-markdown-tests--prefix-overlays)
                      (lambda (a b) (< (overlay-start a)
                                       (overlay-start b)))))
           (marker (nth 0 ovs))
           (body   (nth 1 ovs)))
      (should (string-match-p "┌─ NOTE" (overlay-get marker 'display)))
      (should (string-match-p "\\`│ \\'" (overlay-get body   'display))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-bare-blockquote-body-prefix ()
  "A bare `>' blockquote body line gets a `│ ' display overlay,
and reveal suppresses the display when point sits on it."
  (with-temp-buffer
    (insert "> [!IMPORTANT]\n> first\n>\n> second\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (should (re-search-forward (rx bol ">" eol) nil t))
    (let* ((bare-pos (match-beginning 0))
           (ov (cl-find-if
                (lambda (o)
                  (and (eq (overlay-get o 'gfm-pretty-callouts-kind)
                           'body-prefix)
                       (= (overlay-start o) bare-pos)))
                (overlays-at bare-pos))))
      (should ov)
      (should (equal "│ " (overlay-get ov 'display)))
      (should (overlay-get ov 'gfm-pretty-callouts-revealable))
      (should (= 1 (- (overlay-end ov) (overlay-start ov))))
      ;; Reveal exposes the bare `>' when point sits on the line.
      (goto-char bare-pos)
      (gfm-pretty--reveal)
      (should-not (overlay-get ov 'display))
      (should (stringp (overlay-get ov 'gfm-pretty-saved-display)))
      ;; Box edge returns when point leaves.
      (goto-char (point-max))
      (gfm-pretty--reveal)
      (should (equal "│ " (overlay-get ov 'display))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-body-anchor-face-only-bg-and-extend ()
  "Body-line anchor face specifies only `:background' and/or `:extend' so
emphasis faces on buffer text merge through."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let ((bodies (cl-remove-if-not
                   (lambda (ov)
                     (and (overlay-get ov 'gfm-pretty-callouts-anchor)
                          (overlay-get ov 'wrap-prefix)))
                   (overlays-in (point-min) (point-max)))))
      (should bodies)
      (dolist (ov bodies)
        (let ((face (overlay-get ov 'face)))
          (should (listp face))
          (should-not (plist-member face :slant))
          (should-not (plist-member face :weight))
          (should-not (plist-member face :underline))
          (should-not (plist-member face :strike-through))
          (should-not (plist-member face :foreground))
          (should-not (plist-member face :inherit)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-body-inline-markup-faces-merge-through ()
  "Bold/italic/link/inline-code inside a callout body keep their markdown faces."
  (require 'markdown-mode)
  (with-temp-buffer
    (gfm-mode)
    (insert "> [!NOTE]\n> **boldword** *italword* [linktext](https://x) `codeword`\n")
    (gfm-pretty-mode 1)
    (font-lock-ensure)
    (dolist (cell '(("boldword"   markdown-bold-face)
                    ("italword"   markdown-italic-face)
                    ("linktext"   markdown-link-face)
                    ("codeword"   markdown-inline-code-face)))
      (let ((needle (car cell)) (want (cadr cell)))
        (goto-char (point-min))
        (should (search-forward needle nil t))
        (let* ((pos (match-beginning 0))
               (f (get-text-property pos 'face)))
          (should (or (eq f want)
                      (and (listp f) (memq want f)))))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-body-face-does-not-clobber-slant ()
  "Per-type callout body faces must not specify `:slant', so italic from
`markdown-italic-face' merges through.  The body faces are prepended to
body chars with `font-lock-prepend-text-property', so any attribute
they specify sits at the top of the face merge and shadows lower
faces."
  (dolist (face '(gfm-pretty-callouts-note-body-face
                  gfm-pretty-callouts-tip-body-face
                  gfm-pretty-callouts-important-body-face
                  gfm-pretty-callouts-warning-body-face
                  gfm-pretty-callouts-caution-body-face))
    (should (eq (face-attribute face :slant nil) 'unspecified))
    (should (eq (face-attribute face :weight nil) 'unspecified))
    (should (eq (face-attribute face :underline nil) 'unspecified))))

(ert-deftest lang-markdown/gfm-pretty-callouts-blockquote-face-bg-stripped ()
  "`gfm-pretty-callouts--strip-blockquote-face-bg' clears `:background'
and `:extend' so theme-imposed blockquote stripes don't paint through
the rail decoration or past EOL.  Italic / foreground are left alone."
  (require 'markdown-mode)
  (let ((prev-bg (face-attribute 'markdown-blockquote-face :background nil))
        (prev-extend (face-attribute 'markdown-blockquote-face :extend nil)))
    (unwind-protect
        (progn
          (set-face-attribute 'markdown-blockquote-face nil
                              :background "#abcdef"
                              :extend t)
          (gfm-pretty-callouts--strip-blockquote-face-bg)
          (should (eq (face-attribute 'markdown-blockquote-face :background nil)
                      'unspecified))
          (should (eq (face-attribute 'markdown-blockquote-face :extend nil)
                      'unspecified)))
      (set-face-attribute 'markdown-blockquote-face nil
                          :background prev-bg
                          :extend prev-extend))))

(ert-deftest lang-markdown/gfm-pretty-callouts-marker-only-callout-renders-bottom ()
  "A callout with no body lines still gets a bottom border."
  (with-temp-buffer
    (insert "> [!NOTE]\n\nplain.\n")
    (gfm-pretty-mode 1)
    (let* ((after (cl-some (lambda (ov) (overlay-get ov 'after-string))
                           (overlays-in (point-min) (point-max)))))
      (should (and after (string-match-p "└" after))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-reveal-suppresses-display-at-point ()
  "Moving point onto the prefix temporarily clears its display."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (gfm-pretty--reveal)
    (let ((ov (cl-find-if (lambda (o) (overlay-get o 'gfm-pretty-callouts-revealable))
                          (overlays-at (point-min)))))
      (should ov)
      (should-not (overlay-get ov 'display))
      (should (stringp (overlay-get ov 'gfm-pretty-saved-display))))
    ;; Move off the prefix; display restores.
    (goto-char (point-max))
    (gfm-pretty--reveal)
    (let ((ov (cl-find-if (lambda (o) (overlay-get o 'gfm-pretty-callouts-revealable))
                          (overlays-at (point-min)))))
      (should ov)
      (should (stringp (overlay-get ov 'display))))))

;;; Block-discovery cache

(ert-deftest lang-markdown/gfm-pretty-callouts-collect-cache-eq-no-edit ()
  "Two `gfm-pretty--collect' calls on `callouts' with no edits return `eq' lists."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'callouts))
           (a (gfm-pretty--collect d))
           (b (gfm-pretty--collect d)))
      (should (eq a b)))))

(ert-deftest lang-markdown/gfm-pretty-callouts-collect-cache-invalidates-on-edit ()
  "Edits invalidate the engine `:collect-fn' cache for the callouts decorator."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'callouts))
           (before (gfm-pretty--collect d)))
      (goto-char (point-max))
      (insert "\n> [!TIP]\n> More.\n")
      (let ((after (gfm-pretty--collect d)))
        (should-not (eq before after))
        (should (= 2 (length after)))))))

;;; Narrowing-resilient discovery and teardown — callouts

(defun lang-markdown-tests--two-slide-callouts-buffer ()
  "Insert a two-slide buffer with one callout per slide."
  (insert "> [!NOTE]\n> first.\n")
  (insert "\n# slide 2\n\n")
  (insert "> [!TIP]\n> second.\n"))

(ert-deftest lang-markdown/gfm-pretty-callouts-narrowed-rebuild-does-not-signal ()
  "Narrowed callout rebuild over a widened cache must not signal."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-pretty--rebuild (gfm-pretty--get 'callouts)) t)))))

(ert-deftest lang-markdown/gfm-pretty-callouts-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty--rebuild (gfm-pretty--get 'callouts))
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-pretty-callouts))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length (gfm-pretty--state-get 'callouts 'overlays)) on-buffer))))))

;;; Cross-narrow partial-block coverage — overlay set converges across paths

(defun lang-markdown-tests--tagged-source-positions (tag)
  "Return sorted (BEG . END) positions of overlays carrying TAG."
  (sort
   (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
           (cl-remove-if-not (lambda (ov) (overlay-get ov tag))
                             (overlays-in (point-min) (point-max))))
   (lambda (a b) (or (< (car a) (car b))
                     (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild.
Regression net for any future narrowing-scoped optimisation that
re-introduces narrowing-dependent caches/teardown."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-pretty-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-pretty-tables))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty-tables--rebuild)
      (widen)
      (gfm-pretty-tables--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions 'gfm-pretty-tables))))))

(ert-deftest lang-markdown/gfm-pretty-fences-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-pretty-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-pretty-fences))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (widen)
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-pretty-fences))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-pretty-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-pretty-callouts))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty--rebuild (gfm-pretty--get 'callouts))
      (widen)
      (gfm-pretty--rebuild (gfm-pretty--get 'callouts))
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-pretty-callouts))))))

;;; Extend-clip — callouts

(ert-deftest lang-markdown/gfm-pretty-callouts-body-after-string-fills-to-window-edge ()
  "Each callout body line's right-edge after-string ends with a
`(space :align-to right)' tail in the default face — the visual
line is fully painted to the window edge, so an `:extend t' overlay
face like `hl-line' has no past-EOL region left to fill."
  (with-temp-buffer
    (insert "> [!NOTE]\n> body line.\n")
    (gfm-pretty-mode 1)
    (let* ((body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (nl (line-end-position))
           (rhs (cl-find-if
                 (lambda (o)
                   (eq (overlay-get o 'gfm-pretty-callouts-kind) 'body-rhs))
                 (overlays-in body-beg (1+ nl))))
           (after (and rhs (overlay-get rhs 'after-string))))
      (should after)
      (let* ((nl-i (cl-position ?\n after))
             ;; Tail is the last char of the non-trailing-newline
             ;; portion of the after-string (last body line also
             ;; carries the bottom border after a `\n').
             (tail-i (if nl-i (1- nl-i) (1- (length after)))))
        (should (equal '(space :align-to right)
                       (get-text-property tail-i 'display after)))
        (should (eq 'default (get-text-property tail-i 'face after)))))))

;;; Box-width sizing

(ert-deftest lang-markdown/gfm-pretty-callouts-box-width-clamps-to-narrow-window ()
  "A 100-col-content callout in a 60-col window clamps to 60."
  (let ((buf (generate-new-buffer "*gfm-pretty-callouts-narrow-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "> [!NOTE]\n> "
                  (make-string 100 ?x) "\n")
          (gfm-pretty-mode 1)
          (let* ((rhs-overlays
                  (cl-remove-if-not
                   (lambda (o)
                     (eq (overlay-get o 'gfm-pretty-callouts-kind) 'body-rhs))
                   (overlays-in (point-min) (point-max)))))
            ;; A body line wider than (window-max-chars-per-line - 4) takes
            ;; the overflow path; pad string ends in `│'.
            (should rhs-overlays)
            (let ((after (overlay-get (car rhs-overlays) 'after-string)))
              (should (stringp after))
              (should (string-match-p "│" after)))))
      (kill-buffer buf))))

;;; Wrapped right-edge alignment (overflow path)

(ert-deftest lang-markdown/gfm-pretty-callouts-overflow-line-uses-overflow-after-string ()
  "A 200-col body line wraps and the right-edge `│' lands on the wrapped row.
With wrap-prefix `│ ' (2 cols) and box-width = text-width, the closing
`│' must appear in the right-edge after-string of the non-last body
line (last body line's after-string also carries the bottom border)."
  (let ((buf (generate-new-buffer "*gfm-pretty-callouts-overflow-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "> [!NOTE]\n> " (make-string 200 ?a)
                  "\n> trailing line.\n")
          (gfm-pretty-mode 1)
          (let* ((rhs-overlays
                  (cl-remove-if-not
                   (lambda (o)
                     (eq (overlay-get o 'gfm-pretty-callouts-kind) 'body-rhs))
                   (overlays-in (point-min) (point-max))))
                 ;; First body line (200 a's) takes the overflow path; its
                 ;; right-edge after-string is just the padded `│'.
                 (sorted (sort (copy-sequence rhs-overlays)
                               (lambda (a b)
                                 (< (overlay-start a) (overlay-start b)))))
                 (first-rhs (car sorted))
                 (after (overlay-get first-rhs 'after-string)))
            (should after)
            ;; Right-edge string contains `│', followed by the
            ;; window-edge tail (no bottom border on this row).
            (should (string-match-p "│ \\'" after))
            (should (equal '(space :align-to right)
                           (get-text-property (1- (length after))
                                              'display after)))))
      (kill-buffer buf))))

;;; Per-window display overlays

(ert-deftest lang-markdown/gfm-pretty-callouts-per-window-display-overlays ()
  "Buffer in two windows of different widths gets per-window display overlays."
  (let ((buf (generate-new-buffer "*gfm-pretty-callouts-multi-window*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-pretty-callouts))
                                (overlays-in (point-min) (point-max))))
                     (displays (cl-remove-if-not
                                (lambda (o)
                                  (overlay-get o 'gfm-pretty-callouts-display))
                                overlays))
                     (windowed (cl-count-if
                                (lambda (o) (overlay-get o 'window))
                                displays)))
                (should (> (length displays) 0))
                (should (= (length displays) windowed))))
            (delete-window other)))
      (kill-buffer buf))))

;;; Window-state diff reconciliation

(ert-deftest lang-markdown/gfm-pretty-callouts-reconcile-windows-touches-changed-only ()
  "Reconcile replaces only the resized window's display overlays."
  (let ((buf (generate-new-buffer "*gfm-pretty-callouts-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((displays-for
                      (lambda (w)
                        (cl-remove-if-not
                         (lambda (o)
                           (and (overlay-get o 'gfm-pretty-callouts-display)
                                (eq (overlay-get o 'window) w)))
                         (gfm-pretty--state-get 'callouts 'overlays))))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                (gfm-pretty--state-set 'callouts 'last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              (gfm-pretty--state-get 'callouts 'last-window-state)))
                (gfm-pretty--rebuild-block-for-window (gfm-pretty--get 'callouts)
                 (car (gfm-pretty-callouts--collect-blocks)) win-a)
                (let ((a-after (funcall displays-for win-a))
                      (b-after (funcall displays-for win-b)))
                  (should-not (cl-intersection a-before a-after))
                  (should (= (length b-before) (length b-after)))
                  (should (cl-every (lambda (o) (memq o b-after))
                                    b-before)))))
            (delete-window win-b)))
      (kill-buffer buf))))

;;; Visible-first prioritisation

(ert-deftest lang-markdown/gfm-pretty-callouts-block-visible-p ()
  "`gfm-pretty-callouts--block-visible-p' detects overlap with any window range."
  (let ((block (gfm-pretty-callouts--make-block
                :range (cons 100 200) :payload nil)))
    (should (gfm-pretty-callouts--block-visible-p block '((50 . 250))))
    (should (gfm-pretty-callouts--block-visible-p block '((130 . 180))))
    (should-not (gfm-pretty-callouts--block-visible-p block '((1 . 99) (201 . 300))))
    (should-not (gfm-pretty-callouts--block-visible-p block nil))))

;;; Scoped post-edit rebuild

(defun gfm-pretty-callouts--test-overlay-set ()
  "Return gfm-pretty-callouts overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-pretty-callouts)
        (puthash ov t set)))
    set))

(ert-deftest lang-markdown/gfm-pretty-callouts-scoped-edit-inside-single-block ()
  "Editing inside one callout's body only rebuilds that callout."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n\n> [!TIP]\n> Pizza.\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-callouts--collect-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (r1 (gfm-pretty-callouts--block-range b1))
           (r2 (gfm-pretty-callouts--block-range b2))
           (collect (lambda (range)
                      (cl-remove-if-not
                       (lambda (o) (overlay-get o 'gfm-pretty-callouts))
                       (overlays-in (car range) (cdr range)))))
           (b2-before (funcall collect r2)))
      ;; Edit inside b1 body (line 2 of b1 — past the marker line).
      (save-excursion
        (goto-char (car r1))
        (forward-line 1)
        (let ((p (1+ (point))))
          (gfm-pretty--state-set 'callouts 'dirty-region (cons p p))))
      (gfm-pretty--scheduled-rebuild)
      (let ((b2-after (funcall collect r2)))
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-scoped-edit-on-marker-full-rebuild ()
  "Edit on the marker line triggers a full rebuild."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-callouts--find-blocks))
           (marker-beg (nth 0 (car blocks)))
           (marker-line-end (save-excursion
                              (goto-char marker-beg) (line-end-position)))
           (before (gfm-pretty-callouts--test-overlay-set)))
      (gfm-pretty--state-set 'callouts 'dirty-region
            (cons marker-beg marker-line-end))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-callouts--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-scoped-edit-adjacent-full-rebuild ()
  "Edit on a line adjacent to a callout triggers full rebuild."
  (with-temp-buffer
    (insert "preamble.\n> [!NOTE]\n> Hello.\nepilogue.\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-callouts--find-blocks))
           (block-beg (nth 0 (car blocks)))
           (above-beg (save-excursion
                        (goto-char block-beg) (forward-line -1)
                        (line-beginning-position)))
           (above-end (save-excursion
                        (goto-char block-beg) (forward-line -1)
                        (line-end-position)))
           (before (gfm-pretty-callouts--test-overlay-set)))
      (gfm-pretty--state-set 'callouts 'dirty-region (cons above-beg above-end))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-callouts--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-scoped-edit-outside-blocks-noop ()
  "Edit outside every decorated callout is a no-op."
  (with-temp-buffer
    (insert "intro line.\n\nmore.\n\n> [!NOTE]\n> Hello.\n")
    (gfm-pretty-mode 1)
    (let ((before (gfm-pretty-callouts--test-overlay-set)))
      (gfm-pretty--state-set 'callouts 'dirty-region (cons 1 5))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-callouts--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)))))

;;; Integration: hang regression with cursor-intangible-mode active

(ert-deftest lang-markdown/gfm-pretty-callouts-coexists-with-cursor-intangible-mode ()
  "Enabling callouts in a buffer with cursor-intangible-mode must not hang.
Reproduces the regression where `forward-line' inside the
overlay-creation loop stalls on cursor-intangible/display props
when both `gfm-pretty-callouts-mode' and `gfm-pretty-fences-mode' are active."
  (with-temp-buffer
    (cursor-intangible-mode 1)
    (dotimes (i 5)
      (insert (format "## Section %d\n\nLead.\n\n> [!IMPORTANT]\n> Body %d line a.\n> Body %d line b.\n\n    indented code line %d\n    second indent line\n\nMore text after.\n\n"
                      i i i i)))
    (let ((res (with-timeout (5 'timeout)
                 (gfm-pretty-mode 1)
                 (gfm-pretty-mode 1)
                 'ok)))
      (should (eq res 'ok))
      (should (cl-some (lambda (o) (overlay-get o 'gfm-pretty-callouts))
                       (overlays-in (point-min) (point-max))))
      (should (cl-some (lambda (o) (overlay-get o 'gfm-pretty-fences))
                       (overlays-in (point-min) (point-max)))))))

;;; Per-window cursor reveal

(ert-deftest lang-markdown/gfm-pretty-callouts-reveal-respects-window-restriction ()
  "Reveal in window A doesn't expose source via window B's overlays."
  (let ((buf (generate-new-buffer "*gfm-pretty-callouts-reveal-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (with-selected-window win-a
                (goto-char (point-min))
                (gfm-pretty--reveal))
              (let* ((revealable
                      (cl-remove-if-not
                       (lambda (o)
                         (and (overlay-get o 'gfm-pretty-callouts-revealable)
                              (= (overlay-start o) (point-min))))
                       (gfm-pretty--state-get 'callouts 'overlays)))
                     (a-ov (cl-find-if
                            (lambda (o) (eq (overlay-get o 'window) win-a))
                            revealable))
                     (b-ov (cl-find-if
                            (lambda (o) (eq (overlay-get o 'window) win-b))
                            revealable)))
                (should a-ov)
                (should b-ov)
                (should-not (overlay-get a-ov 'display))
                (should (stringp (overlay-get b-ov 'display)))))
            (delete-window win-b)))
      (kill-buffer buf))))

;;; gfm-pretty-fences tests

(require 'gfm-pretty-fences)

(ert-deftest lang-markdown/gfm-pretty-fences-find-block ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (let ((blocks (gfm-pretty-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should (equal "bash" (nth 4 (car blocks)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-find-block-no-lang ()
  (with-temp-buffer
    (insert "```\ntext\n```\n")
    (let ((blocks (gfm-pretty-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should-not (nth 4 (car blocks))))))

(ert-deftest lang-markdown/gfm-pretty-fences-mode-creates-overlays ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-fences))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-fences-mode-removes-overlays ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (gfm-pretty-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-fences))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-fences-enabled-via-gfm-mode-hook ()
  (should (memq 'gfm-pretty-mode gfm-mode-hook)))

;;; Narrowing-resilient discovery and teardown — code fences

(defun lang-markdown-tests--two-slide-fences-buffer ()
  "Insert a two-slide buffer with one fenced code block per slide."
  (insert "```bash\necho one\n```\n")
  (insert "\n# slide 2\n\n")
  (insert "```bash\necho two\n```\n"))

(ert-deftest lang-markdown/gfm-pretty-fences-narrowed-rebuild-does-not-signal ()
  "Narrowed rebuild of fences over a widened cache must not signal."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-pretty--rebuild (gfm-pretty--get 'fences)) t)))))

(ert-deftest lang-markdown/gfm-pretty-fences-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-pretty-fences))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length (gfm-pretty--state-get 'fences 'overlays)) on-buffer))))))

;;; Body background fill — code fences

(defun lang-markdown-tests--fence-body-after-string (body-beg lend)
  "Return the body display overlay's `after-string' for line [BODY-BEG, LEND]."
  (let ((ov (cl-find-if
             (lambda (o)
               (eq (overlay-get o 'gfm-pretty-fences-kind) 'body))
             (overlays-in body-beg (1+ lend)))))
    (and ov (overlay-get ov 'after-string))))

(ert-deftest lang-markdown/gfm-pretty-fences-diff-body-bg-fills-gap ()
  "A `+' body line's right-edge padding carries the `:extend t' background."
  (with-temp-buffer
    (insert "```diff\n+ x\n```\n")
    (gfm-pretty-mode 1)
    ;; Stand in for native fontification copying diff-mode's `:extend t'
    ;; `diff-added' face onto the `+' body line — an explicit plist,
    ;; since batch Emacs does not realise a defface's `:extend'.
    (let* ((diff-added-face '(:background "#c3ebc1" :extend t))
           (body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (lend (line-end-position)))
      (put-text-property body-beg (1+ lend) 'face diff-added-face)
      ;; Rebuild so the display pass reads the freshly-applied face.
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (let ((after (lang-markdown-tests--fence-body-after-string
                    body-beg lend)))
        (should after)
        ;; The padding (first char of the after-string) is painted with
        ;; the diff background.
        (should (equal "#c3ebc1"
                       (plist-get (get-text-property 0 'face after)
                                  :background)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-plain-body-no-bg-fill ()
  "A body line with no `:extend t' background paints the padding with system bg.
The padding face carries `:background \"unspecified-bg\"' so the
after-string actively paints the system background instead of
inheriting (and bleeding through) the buffer text-property face's
`:background' at the line's newline."
  (with-temp-buffer
    (insert "```text\nplain line\n```\n")
    (gfm-pretty-mode 1)
    (let* ((body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (lend (line-end-position))
           (after (lang-markdown-tests--fence-body-after-string
                   body-beg lend)))
      (should after)
      (should (equal "unspecified-bg"
                     (plist-get (get-text-property 0 'face after)
                                :background))))))

(ert-deftest lang-markdown/gfm-pretty-fences-line-extend-bg-ignores-non-extending ()
  "`gfm-pretty-fences--line-extend-bg' ignores a `:background' without `:extend t'."
  (with-temp-buffer
    (insert "code line\n")
    ;; A `:background' without `:extend t' must not fill the gap.
    (put-text-property (point-min) (1- (point-max))
                       'face '(:background "#abcdef"))
    (should-not (gfm-pretty-fences--line-extend-bg (point-min)
                                                 (1- (point-max))))
    ;; A face specifying both is picked up.
    (put-text-property (point-min) (1- (point-max))
                       'face '(:background "#abcdef" :extend t))
    (should (equal "#abcdef"
                   (gfm-pretty-fences--line-extend-bg (point-min)
                                                    (1- (point-max)))))))

;;; Selection-aware decoration swap — fences
;;
;; Behavioural contract (driven by user feedback 2026-05-21):
;;
;; - Only `V'-line (linewise) evil visual selection swaps decoration
;;   overlays to the bare (region-bg-painted) variant.  Every line in
;;   the V-line range gets its decorations painted with `region' bg
;;   end-to-end across the window — no exceptions for box-top, body,
;;   or box-bottom edges.
;;
;; - Charwise (`v') visual, visual-block (`C-v'), vanilla `mark-active'
;;   regions, and the no-selection state all leave decoration overlays
;;   in their masked variant.  Emacs paints `region' bg on the buffer
;;   chars they cover; the box decorations stay opaque.
;;
;; - The swap follows a per-line rule (overlay-start's line is in the
;;   V-line range), not a position-overlap rule.  Zero-width overlays
;;   anchored at EOL belong to that line.

(defvar evil-state)
(defvar evil-visual-selection)
(defvar evil-visual-beginning)
(defvar evil-visual-end)

(defconst lang-markdown-tests--fences-selection-buffer
  "para before\n\n```lisp\nbody one\nbody two\nbody three\n```\n\npara after\n"
  "Test buffer for V-line selection scenarios.
Line 1: para before
Line 2: (empty)
Line 3: ```lisp     (open fence)
Line 4: body one
Line 5: body two
Line 6: body three
Line 7: ```         (close fence)
Line 8: (empty)
Line 9: para after")

(defun lang-markdown-tests--line-bol (n)
  "Return position of BOL of line N (1-indexed)."
  (save-excursion (goto-char (point-min)) (forward-line (1- n)) (point)))

(defun lang-markdown-tests--line-eol (n)
  (save-excursion (goto-char (point-min)) (forward-line (1- n)) (line-end-position)))

(defun lang-markdown-tests--overlay-variant (ov)
  "Return the swap variant for OV: `bare', `masked', `mixed', or nil.
`mixed' is returned when OV stashes multiple swap props that resolve
to different variants — e.g. a fences body overlay whose
before-string is bare (lbeg in selection) but after-string is masked
\(lend past selection end)."
  (let ((variants
         (delq nil
               (mapcar
                (lambda (entry)
                  (let* ((prop (car entry))
                         (base (symbol-name (cdr entry)))
                         (masked (overlay-get
                                  ov (intern (concat base "-masked")))))
                    (when masked
                      (let ((cur (overlay-get ov prop))
                            (bare (overlay-get
                                   ov (intern (concat base "-bare")))))
                        (cond ((eq cur bare) 'bare)
                              ((eq cur masked) 'masked)
                              (t 'unknown))))))
                gfm-pretty--variant-props))))
    (cond
     ((null variants) nil)
     ((cl-every (lambda (v) (eq v 'bare)) variants) 'bare)
     ((cl-every (lambda (v) (eq v 'masked)) variants) 'masked)
     (t 'mixed))))

(defun lang-markdown-tests--fence-variants-by-line ()
  "Return alist `(LINE . ((KIND . VARIANT) ...))' for all fence decorations.
Each KIND appears once per overlay; lines sorted ascending."
  (let ((by-line (make-hash-table :test 'equal)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((kind (overlay-get ov 'gfm-pretty-fences-kind))
            (variant (lang-markdown-tests--overlay-variant ov)))
        (when (and kind variant)
          (let ((line (line-number-at-pos (overlay-start ov))))
            (push (cons kind variant) (gethash line by-line))))))
    (let (result)
      (maphash (lambda (k v)
                 (push (cons k (sort (copy-sequence v)
                                     (lambda (a b)
                                       (string< (symbol-name (car a))
                                                (symbol-name (car b))))))
                       result))
               by-line)
      (sort result (lambda (a b) (< (car a) (car b)))))))

(defmacro lang-markdown-tests--with-fences-test-buffer (&rest body)
  "Insert the standard fences test buffer, enable mode, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (insert lang-markdown-tests--fences-selection-buffer)
     (goto-char (point-min))
     (gfm-pretty-mode 1)
     ,@body))

(defmacro lang-markdown-tests--with-evil-v-line (line-beg line-end &rest body)
  "Mimic evil V-line state covering LINE-BEG through LINE-END, run BODY.
Forces a walker re-run by clearing the memoised last-bounds."
  (declare (indent 2))
  `(dlet ((mark-active nil)
          (evil-state 'visual)
          (evil-visual-selection 'line)
          (evil-visual-beginning
           (copy-marker (lang-markdown-tests--line-bol ,line-beg)))
          (evil-visual-end
           (copy-marker (lang-markdown-tests--line-bol (1+ ,line-end)))))
     (setq gfm-pretty--last-selection-bounds nil)
     (gfm-pretty--update-selection)
     ,@body))

(defmacro lang-markdown-tests--with-evil-v-char (beg-pos end-pos &rest body)
  "Mimic evil v charwise state from BEG-POS to END-POS, run BODY."
  (declare (indent 2))
  `(dlet ((mark-active nil)
          (evil-state 'visual)
          (evil-visual-selection 'char)
          (evil-visual-beginning (copy-marker ,beg-pos))
          (evil-visual-end (copy-marker ,end-pos)))
     (setq gfm-pretty--last-selection-bounds nil)
     (gfm-pretty--update-selection)
     ,@body))

(defmacro lang-markdown-tests--with-vanilla-region (beg-pos end-pos &rest body)
  "Activate a vanilla `mark-active' region from BEG-POS to END-POS, run BODY."
  (declare (indent 2))
  `(progn
     (set-mark ,beg-pos)
     (goto-char ,end-pos)
     (setq deactivate-mark nil)
     (activate-mark)
     (setq gfm-pretty--last-selection-bounds nil)
     (gfm-pretty--update-selection)
     ,@body))

;;; selection-bounds helper

(ert-deftest lang-markdown/gfm-pretty-fences-selection-bounds-v-line-returns-range ()
  "Evil V-line state returns the line-aligned marker range."
  (with-temp-buffer
    (insert "hello\nworld\n")
    (dlet ((mark-active nil)
           (evil-state 'visual)
           (evil-visual-selection 'line)
           (evil-visual-beginning (copy-marker 1))
           (evil-visual-end (copy-marker 7)))
      (should (equal (cons 1 7) (gfm-pretty--selection-bounds))))))

(ert-deftest lang-markdown/gfm-pretty-fences-selection-bounds-charwise-returns-full-range ()
  "Charwise (`v') visual returns the full evil-visual marker range.
The per-overlay walker then decides which overlays are fully inside."
  (with-temp-buffer
    (insert "hello world\n")
    (dlet ((mark-active nil)
           (evil-state 'visual)
           (evil-visual-selection 'char)
           (evil-visual-beginning (copy-marker 2))
           (evil-visual-end (copy-marker 8)))
      (should (equal (cons 2 8) (gfm-pretty--selection-bounds))))))

(ert-deftest lang-markdown/gfm-pretty-fences-selection-bounds-block-returns-nil ()
  "Evil visual-block does not drive decoration swap."
  (with-temp-buffer
    (insert "hello\n")
    (dlet ((mark-active nil)
           (evil-state 'visual)
           (evil-visual-selection 'block)
           (evil-visual-beginning (copy-marker 1))
           (evil-visual-end (copy-marker 6)))
      (should-not (gfm-pretty--selection-bounds)))))

(ert-deftest lang-markdown/gfm-pretty-fences-selection-bounds-vanilla-returns-full-range ()
  "Vanilla `mark-active' region returns the full `region-beginning'/`-end' range."
  (with-temp-buffer
    (insert "hello world\n")
    (transient-mark-mode 1)
    (push-mark 2 t t)
    (goto-char 8)
    (should (use-region-p))
    (should (equal (cons 2 8) (gfm-pretty--selection-bounds)))))

(ert-deftest lang-markdown/gfm-pretty-fences-selection-bounds-no-region-nil ()
  "With no selection active, bounds are nil."
  (with-temp-buffer
    (insert "hello\n")
    (should-not (gfm-pretty--selection-bounds))))

;;; Decoration variant stashing

(ert-deftest lang-markdown/gfm-pretty-fences-decoration-stashes-both-variants ()
  "Every fence decoration overlay stashes both masked and bare variants."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((decoration-kinds '(top-leading top-trailing body
                              bottom-leading bottom-trailing)))
      (dolist (kind decoration-kinds)
        (let ((ovs (cl-remove-if-not
                    (lambda (o)
                      (eq (overlay-get o 'gfm-pretty-fences-kind) kind))
                    (overlays-in (point-min) (point-max)))))
          (should ovs)
          (dolist (ov ovs)
            (should (cl-some
                     (lambda (entry)
                       (let ((base (symbol-name (cdr entry))))
                         (and (overlay-get
                               ov (intern (concat base "-masked")))
                              (overlay-get
                               ov (intern (concat base "-bare"))))))
                     gfm-pretty--variant-props))))))))

;;; V-line scenarios

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-on-open-fence-paints-top-only ()
  "V-line on the open fence line: only top-leading and top-trailing are bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 3
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body . masked))
                 (5 (body . masked))
                 (6 (body . masked))
                 (7 (bottom-leading . masked) (bottom-trailing . masked)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-on-one-body-line-paints-that-body-only ()
  "V-line on one body line: only that line's body decoration is bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 5 5
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body . masked))
                 (5 (body . bare))
                 (6 (body . masked))
                 (7 (bottom-leading . masked) (bottom-trailing . masked)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-on-close-fence-paints-bottom-only ()
  "V-line on the close fence line: only bottom-leading and bottom-trailing bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 7 7
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body . masked))
                 (5 (body . masked))
                 (6 (body . masked))
                 (7 (bottom-leading . bare) (bottom-trailing . bare)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-whole-box-paints-everything ()
  "V-line over the entire box: every decoration is bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 7
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body . bare))
                 (5 (body . bare))
                 (6 (body . bare))
                 (7 (bottom-leading . bare) (bottom-trailing . bare)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-outside-box-paints-nothing ()
  "V-line outside the box: every decoration stays masked."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 1 2
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body . masked))
                 (5 (body . masked))
                 (6 (body . masked))
                 (7 (bottom-leading . masked) (bottom-trailing . masked)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-overlapping-open-and-body ()
  "V-line covering open fence + one body line: top + that body bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 4
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body . bare))
                 (5 (body . masked))
                 (6 (body . masked))
                 (7 (bottom-leading . masked) (bottom-trailing . masked)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-line-overlapping-body-and-close ()
  "V-line covering last body line + close fence: that body + bottom bare."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 6 7
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body . masked))
                 (5 (body . masked))
                 (6 (body . bare))
                 (7 (bottom-leading . bare) (bottom-trailing . bare)))
               (lang-markdown-tests--fence-variants-by-line))))))

;;; v / charwise / vanilla — no decoration paint

(ert-deftest lang-markdown/gfm-pretty-fences-v-char-single-line-leaves-masked ()
  "Charwise selection within one body line: nothing painted."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((body4-bol (lang-markdown-tests--line-bol 4)))
      (lang-markdown-tests--with-evil-v-char (+ body4-bol 2) (+ body4-bol 6)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body . masked))
                   (5 (body . masked))
                   (6 (body . masked))
                   (7 (bottom-leading . masked) (bottom-trailing . masked)))
                 (lang-markdown-tests--fence-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-char-multi-line-paints-interior ()
  "Charwise from mid-L4 to mid-L6: interior L5 body fully bare, L4/L6 mixed.
L4 (start line): before-string masked (lbeg before region-beg),
after-string bare (lend in region) → `mixed'.
L5 (interior): both bare → `bare'.
L6 (end line): before-string bare (lbeg in region), after-string
masked (lend past region-end) → `mixed'."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((body4-bol (lang-markdown-tests--line-bol 4))
          (body6-bol (lang-markdown-tests--line-bol 6)))
      (lang-markdown-tests--with-evil-v-char (+ body4-bol 2) (+ body6-bol 4)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body . mixed))
                   (5 (body . bare))
                   (6 (body . mixed))
                   (7 (bottom-leading . masked) (bottom-trailing . masked)))
                 (lang-markdown-tests--fence-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-char-end-line-lhs-paints-when-covered ()
  "End-line of v-charwise: the left `│ ' (body before-string) paints bare
when the selection covers its position at BOL, even though the body
overlay extends past the selection end (so the right edge stays masked
— we want per-prop selection, not a single overlay-wide swap).

Regression: a v-charwise from above the fenced block into the body
left the end-line's left border masked, creating a 2-col cream gap
at BOL where buffer chars on that line were already region-painted.
The test asserts that the body overlay's `before-string' is the bare
variant once the selection covers its anchor at lbeg."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((l1-bol (lang-markdown-tests--line-bol 1))
          (l5-bol (lang-markdown-tests--line-bol 5)))
      ;; v from start of paragraph above (L1) to mid-body L5.
      (lang-markdown-tests--with-evil-v-char l1-bol (+ l5-bol 3)
        ;; The body overlay on L5 (selection's end-line): the before-
        ;; string at lbeg is fully inside the selection range, so it
        ;; should paint bare; the after-string at lend is past the
        ;; selection end so it stays masked.
        (let* ((body-ov (cl-find-if
                         (lambda (o)
                           (and (eq (overlay-get o 'gfm-pretty-fences-kind)
                                    'body)
                                (= (line-number-at-pos (overlay-start o)) 5)))
                         (overlays-in (point-min) (point-max)))))
          (should body-ov)
          (should (eq (overlay-get body-ov 'gfm-pretty-before-bare)
                      (overlay-get body-ov 'before-string)))
          (should (eq (overlay-get body-ov 'gfm-pretty-after-masked)
                      (overlay-get body-ov 'after-string))))))))

(ert-deftest lang-markdown/gfm-pretty-fences-v-char-spans-top-and-body ()
  "Charwise from mid-L3 (open fence) to mid-L5 (body): per-overlay containment.
The open fence's `top-trailing' is zero-width at L3-eol which falls
inside the selection, so it goes bare; `top-leading' (full-line range)
isn't contained because L3-bol precedes the selection start, so it
stays masked.  Body L4 (fully inside) is bare; body L5 is mixed —
its before-string at lbeg is inside the selection (left `│ ' paints),
but its after-string at lend is past the selection end."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((l3-bol (lang-markdown-tests--line-bol 3))
          (l5-bol (lang-markdown-tests--line-bol 5)))
      (lang-markdown-tests--with-evil-v-char (+ l3-bol 1) (+ l5-bol 3)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . bare))
                   (4 (body . bare))
                   (5 (body . mixed))
                   (6 (body . masked))
                   (7 (bottom-leading . masked) (bottom-trailing . masked)))
                 (lang-markdown-tests--fence-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-vanilla-single-line-leaves-masked ()
  "Vanilla region within one body line: nothing painted."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((body4-bol (lang-markdown-tests--line-bol 4)))
      (lang-markdown-tests--with-vanilla-region
          (+ body4-bol 2) (+ body4-bol 6)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body . masked))
                   (5 (body . masked))
                   (6 (body . masked))
                   (7 (bottom-leading . masked) (bottom-trailing . masked)))
                 (lang-markdown-tests--fence-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-vanilla-multi-line-paints-interior ()
  "Vanilla region across L4-L6 paints L5 interior body bare; L4/L6 mixed.
Same per-overlay containment as charwise: the start and end body
overlays' before/after-strings swap independently."
  (lang-markdown-tests--with-fences-test-buffer
    (let ((body4-bol (lang-markdown-tests--line-bol 4))
          (body6-bol (lang-markdown-tests--line-bol 6)))
      (lang-markdown-tests--with-vanilla-region
          (+ body4-bol 2) (+ body6-bol 4)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body . mixed))
                   (5 (body . bare))
                   (6 (body . mixed))
                   (7 (bottom-leading . masked) (bottom-trailing . masked)))
                 (lang-markdown-tests--fence-variants-by-line)))))))

;;; Deselection restores masked

(ert-deftest lang-markdown/gfm-pretty-fences-deselection-restores-masked ()
  "After V-line + deselect, every decoration returns to masked."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 7
      ;; (V-line setup painted bare; below the macro exit, simulate
      ;; deselection by running the walker outside the dlet so the
      ;; bounds-change detection unwinds to masked.)
      (ignore))
    (gfm-pretty--update-selection)
    (should (equal
             '((3 (top-leading . masked) (top-trailing . masked))
               (4 (body . masked))
               (5 (body . masked))
               (6 (body . masked))
               (7 (bottom-leading . masked) (bottom-trailing . masked)))
             (lang-markdown-tests--fence-variants-by-line)))))

;;; Rebuild during active V-line

(defun lang-markdown-tests--overlay-variant-on-line (line kind)
  "Return variant for decoration overlay of KIND on LINE, or nil."
  (when-let* ((ov (cl-find-if
                   (lambda (o)
                     (and (eq (overlay-get o 'gfm-pretty-fences-kind) kind)
                          (= (line-number-at-pos (overlay-start o)) line)))
                   (overlays-in (point-min) (point-max)))))
    (lang-markdown-tests--overlay-variant ov)))

(ert-deftest lang-markdown/gfm-pretty-fences-scroll-back-into-view-resets-stale-bare ()
  "An overlay marked bare while off-screen resets when scrolled back into view.
Regression: the bounds-memoised walker would early-return when current
bounds matched last-seen bounds, leaving overlays that scrolled out of
the visible window — and so weren't touched during the previous walk
— stuck in their prior variant once scrolled back in."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 5 5
      (should (eq 'bare
                  (lang-markdown-tests--overlay-variant-on-line 5 'body))))
    ;; Simulate a scroll that hides line 5 from the visible range, then
    ;; run the walker (with the V-line already gone since the dlet
    ;; popped).  L5 is off-screen so it should not be updated this pass.
    (cl-letf (((symbol-function 'gfm-pretty--visible-window-ranges)
               (lambda ()
                 (list (cons (point-min)
                             (lang-markdown-tests--line-bol 4))))))
      (gfm-pretty--update-selection))
    ;; Now the window scrolls so L5 is visible again.  The selection
    ;; bounds are still nil (no change), but L5 was missed last walk
    ;; and is still bare — the walker must update it.
    (gfm-pretty--update-selection)
    (should (eq 'masked
                (lang-markdown-tests--overlay-variant-on-line 5 'body)))))

(ert-deftest lang-markdown/gfm-pretty-fences-rebuild-during-v-line-paints-bare ()
  "A decorator rebuild with V-line active creates overlays already in bare state."
  (lang-markdown-tests--with-fences-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 7
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body . bare))
                 (5 (body . bare))
                 (6 (body . bare))
                 (7 (bottom-leading . bare) (bottom-trailing . bare)))
               (lang-markdown-tests--fence-variants-by-line))))))

;;; Selection-aware decoration swap — callouts

(defconst lang-markdown-tests--callouts-selection-buffer
  "para before\n\n> [!NOTE]\n> body one\n> body two\n> body three\n\npara after\n"
  "Test buffer for V-line selection scenarios on callouts.
Line 1: para before
Line 2: (empty)
Line 3: > [!NOTE]      (marker)
Line 4: > body one
Line 5: > body two
Line 6: > body three   (last body)
Line 7: (empty)
Line 8: para after")

(defmacro lang-markdown-tests--with-callouts-test-buffer (&rest body)
  "Insert the standard callouts test buffer, enable mode, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (insert lang-markdown-tests--callouts-selection-buffer)
     (goto-char (point-min))
     (gfm-pretty-mode 1)
     ,@body))

(defun lang-markdown-tests--callout-variants-by-line ()
  "Return alist `(LINE . ((KIND . VARIANT) ...))' for callout decorations."
  (let ((by-line (make-hash-table :test 'equal)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((kind (overlay-get ov 'gfm-pretty-callouts-kind))
            (variant (lang-markdown-tests--overlay-variant ov)))
        (when (and kind variant)
          (let ((line (line-number-at-pos (overlay-start ov))))
            (push (cons kind variant) (gethash line by-line))))))
    (let (result)
      (maphash (lambda (k v)
                 (push (cons k (sort (copy-sequence v)
                                     (lambda (a b)
                                       (string< (symbol-name (car a))
                                                (symbol-name (car b))))))
                       result))
               by-line)
      (sort result (lambda (a b) (< (car a) (car b)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-decoration-stashes-both-variants ()
  "Every callout decoration overlay stashes both masked and bare variants."
  (lang-markdown-tests--with-callouts-test-buffer
    (dolist (kind '(top-leading top-trailing body-prefix body-rhs))
      (let ((ovs (cl-remove-if-not
                  (lambda (o)
                    (eq (overlay-get o 'gfm-pretty-callouts-kind) kind))
                  (overlays-in (point-min) (point-max)))))
        (should ovs)
        (dolist (ov ovs)
          (should (cl-some
                   (lambda (entry)
                     (let ((base (symbol-name (cdr entry))))
                       (and (overlay-get
                             ov (intern (concat base "-masked")))
                            (overlay-get
                             ov (intern (concat base "-bare"))))))
                   gfm-pretty--variant-props)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-line-on-marker-paints-top-only ()
  "V-line on the callout marker line: only top decorations bare."
  (lang-markdown-tests--with-callouts-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 3
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body-prefix . masked) (body-rhs . masked))
                 (5 (body-prefix . masked) (body-rhs . masked))
                 (6 (body-bottom . masked)
                  (body-prefix . masked) (body-rhs . masked)))
               (lang-markdown-tests--callout-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-line-on-one-body-paints-that-body-only ()
  "V-line on one callout body line: only that line's decorations bare."
  (lang-markdown-tests--with-callouts-test-buffer
    (lang-markdown-tests--with-evil-v-line 5 5
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body-prefix . masked) (body-rhs . masked))
                 (5 (body-prefix . bare) (body-rhs . bare))
                 (6 (body-bottom . masked)
                  (body-prefix . masked) (body-rhs . masked)))
               (lang-markdown-tests--callout-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-line-whole-box-keeps-bottom-masked ()
  "V-line over the entire callout: top + body bare, but `body-bottom'
stays masked because its select-range is the line BELOW the box and
the V-line stops at the last body line.  A V-line that extends one
more line down (covered by the next test) paints the bottom too."
  (lang-markdown-tests--with-callouts-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 6
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body-prefix . bare) (body-rhs . bare))
                 (5 (body-prefix . bare) (body-rhs . bare))
                 (6 (body-bottom . masked)
                  (body-prefix . bare) (body-rhs . bare)))
               (lang-markdown-tests--callout-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-line-extends-past-paints-bottom ()
  "V-line extending one line past the callout paints the bottom border bare.
Regression: the box's bottom edge needs region bg when the selection
crosses past the last body line into the line below."
  (lang-markdown-tests--with-callouts-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 7
      (should (equal
               '((3 (top-leading . bare) (top-trailing . bare))
                 (4 (body-prefix . bare) (body-rhs . bare))
                 (5 (body-prefix . bare) (body-rhs . bare))
                 (6 (body-bottom . bare)
                  (body-prefix . bare) (body-rhs . bare)))
               (lang-markdown-tests--callout-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-line-outside-paints-nothing ()
  "V-line outside the callout: all decorations stay masked."
  (lang-markdown-tests--with-callouts-test-buffer
    (lang-markdown-tests--with-evil-v-line 1 2
      (should (equal
               '((3 (top-leading . masked) (top-trailing . masked))
                 (4 (body-prefix . masked) (body-rhs . masked))
                 (5 (body-prefix . masked) (body-rhs . masked))
                 (6 (body-bottom . masked)
                  (body-prefix . masked) (body-rhs . masked)))
               (lang-markdown-tests--callout-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-char-multi-line-per-overlay ()
  "Charwise from mid-L4 to mid-L6: per-overlay containment decides each.
- L4 (start line): `body-prefix' masked (LHS chars precede the
  selection start), `body-rhs' bare (zero-width at L4-eol falls inside
  the range, so the right edge paints past EOL).
- L5 (interior): both bare.
- L6 (end line): `body-prefix' bare (selection extends past the LHS
  chars), `body-rhs' masked (L6-eol is past the selection end),
  `body-bottom' masked (selection doesn't reach past the callout)."
  (lang-markdown-tests--with-callouts-test-buffer
    (let ((l4-bol (lang-markdown-tests--line-bol 4))
          (l6-bol (lang-markdown-tests--line-bol 6)))
      (lang-markdown-tests--with-evil-v-char (+ l4-bol 3) (+ l6-bol 4)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body-prefix . masked) (body-rhs . bare))
                   (5 (body-prefix . bare) (body-rhs . bare))
                   (6 (body-bottom . masked)
                    (body-prefix . bare) (body-rhs . masked)))
                 (lang-markdown-tests--callout-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-char-end-line-lhs-paints-when-covered ()
  "End-line `body-prefix' paints bare when the selection covers its `> ' chars.
Regression: a v-charwise selection extending past the LHS `> ' on
the final line used to leave the left border masked, breaking the
visual continuity of the box border on selected rows."
  (lang-markdown-tests--with-callouts-test-buffer
    (let ((l4-bol (lang-markdown-tests--line-bol 4))
          (l5-bol (lang-markdown-tests--line-bol 5)))
      (lang-markdown-tests--with-evil-v-char (+ l4-bol 3) (+ l5-bol 5)
        (should (eq 'bare
                    (cdr (assq 'body-prefix
                               (cdr (assq 5 (lang-markdown-tests--callout-variants-by-line)))))))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-v-char-single-line-leaves-masked ()
  "Charwise within one body line: nothing painted."
  (lang-markdown-tests--with-callouts-test-buffer
    (let ((l5-bol (lang-markdown-tests--line-bol 5)))
      (lang-markdown-tests--with-evil-v-char (+ l5-bol 3) (+ l5-bol 7)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body-prefix . masked) (body-rhs . masked))
                   (5 (body-prefix . masked) (body-rhs . masked))
                   (6 (body-bottom . masked)
                    (body-prefix . masked) (body-rhs . masked)))
                 (lang-markdown-tests--callout-variants-by-line)))))))

;;; Selection-aware decoration swap — fences indent block

(defconst lang-markdown-tests--indent-selection-buffer
  "para before\n\n    indent line one\n    indent line two\n    indent line three\n\npara after\n"
  "Test buffer for V-line selection scenarios on indent code blocks.
Line 1: para before
Line 2: (empty)
Line 3:     indent line one
Line 4:     indent line two
Line 5:     indent line three   (last body line)
Line 6: (empty)
Line 7: para after")

(defmacro lang-markdown-tests--with-indent-test-buffer (&rest body)
  "Insert the standard indent test buffer, enable mode, run BODY."
  (declare (indent 0))
  `(with-temp-buffer
     (insert lang-markdown-tests--indent-selection-buffer)
     (goto-char (point-min))
     (gfm-pretty-mode 1)
     ,@body))

(defun lang-markdown-tests--indent-variants-by-line (&optional kinds)
  "Return alist `(LINE . ((KIND . VARIANT) ...))' for fences indent overlays.
KINDS limits to specific decoration kinds when non-nil."
  (let ((by-line (make-hash-table :test 'equal))
        (kinds (or kinds '(indent-top indent-rhs indent-bottom))))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((kind (overlay-get ov 'gfm-pretty-fences-kind))
            (variant (lang-markdown-tests--overlay-variant ov)))
        (when (and kind variant (memq kind kinds))
          (let ((line (line-number-at-pos (overlay-start ov))))
            (push (cons kind variant) (gethash line by-line))))))
    (let (result)
      (maphash (lambda (k v)
                 (push (cons k (sort (copy-sequence v)
                                     (lambda (a b)
                                       (string< (symbol-name (car a))
                                                (symbol-name (car b))))))
                       result))
               by-line)
      (sort result (lambda (a b) (< (car a) (car b)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-indent-v-line-on-one-body-paints-rhs-only ()
  "V-line on one indent body line: only that line's indent-rhs is bare."
  (lang-markdown-tests--with-indent-test-buffer
    (lang-markdown-tests--with-evil-v-line 4 4
      (should (equal
               '((3 (indent-rhs . masked) (indent-top . masked))
                 (4 (indent-rhs . bare))
                 (5 (indent-bottom . masked) (indent-rhs . masked)))
               (lang-markdown-tests--indent-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-indent-v-line-whole-block-bottom-stays-masked ()
  "V-line over the whole indent block: indent-bottom stays masked
because its select-range targets the line BELOW the block."
  (lang-markdown-tests--with-indent-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 5
      (should (equal
               '((3 (indent-rhs . bare) (indent-top . bare))
                 (4 (indent-rhs . bare))
                 (5 (indent-bottom . masked) (indent-rhs . bare)))
               (lang-markdown-tests--indent-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-indent-v-line-extends-past-paints-bottom ()
  "V-line extending one line past the indent block paints indent-bottom bare."
  (lang-markdown-tests--with-indent-test-buffer
    (lang-markdown-tests--with-evil-v-line 3 6
      (should (equal
               '((3 (indent-rhs . bare) (indent-top . bare))
                 (4 (indent-rhs . bare))
                 (5 (indent-bottom . bare) (indent-rhs . bare)))
               (lang-markdown-tests--indent-variants-by-line))))))


;;; Selection-aware decoration swap — vanilla on callouts

(ert-deftest lang-markdown/gfm-pretty-callouts-vanilla-single-line-leaves-masked ()
  "Vanilla region within one callout body line: nothing painted."
  (lang-markdown-tests--with-callouts-test-buffer
    (let ((l5-bol (lang-markdown-tests--line-bol 5)))
      (lang-markdown-tests--with-vanilla-region
          (+ l5-bol 3) (+ l5-bol 7)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body-prefix . masked) (body-rhs . masked))
                   (5 (body-prefix . masked) (body-rhs . masked))
                   (6 (body-bottom . masked)
                    (body-prefix . masked) (body-rhs . masked)))
                 (lang-markdown-tests--callout-variants-by-line)))))))

(ert-deftest lang-markdown/gfm-pretty-callouts-vanilla-multi-line-per-overlay ()
  "Vanilla region across L4-L6 paints per-overlay containment."
  (lang-markdown-tests--with-callouts-test-buffer
    (let ((l4-bol (lang-markdown-tests--line-bol 4))
          (l6-bol (lang-markdown-tests--line-bol 6)))
      (lang-markdown-tests--with-vanilla-region
          (+ l4-bol 3) (+ l6-bol 4)
        (should (equal
                 '((3 (top-leading . masked) (top-trailing . masked))
                   (4 (body-prefix . masked) (body-rhs . bare))
                   (5 (body-prefix . bare) (body-rhs . bare))
                   (6 (body-bottom . masked)
                    (body-prefix . bare) (body-rhs . masked)))
                 (lang-markdown-tests--callout-variants-by-line)))))))

;;; Selection-aware decoration swap — YAML helmet

(defconst lang-markdown-tests--yaml-helmet-selection-buffer
  "---\nkey: value\nother: thing\n---\n\npara after\n"
  "Test buffer for V-line selection scenarios on YAML helmet.
Line 1: ---        (open marker)
Line 2: key: value
Line 3: other: thing
Line 4: ---        (close marker)
Line 5: (empty)
Line 6: para after")

(defmacro lang-markdown-tests--with-yaml-helmet-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert lang-markdown-tests--yaml-helmet-selection-buffer)
     (goto-char (point-min))
     (gfm-pretty-mode 1)
     ,@body))

(ert-deftest lang-markdown/gfm-pretty-fences-yaml-helmet-v-line-whole-block-paints-all ()
  "V-line over the whole YAML helmet: every decoration kind bare."
  (lang-markdown-tests--with-yaml-helmet-buffer
    (lang-markdown-tests--with-evil-v-line 1 4
      (should (equal
               '((1 (top-leading . bare) (top-trailing . bare))
                 (2 (body . bare))
                 (3 (body . bare))
                 (4 (bottom-leading . bare) (bottom-trailing . bare)))
               (lang-markdown-tests--fence-variants-by-line))))))

(ert-deftest lang-markdown/gfm-pretty-fences-yaml-helmet-v-line-on-one-body-line ()
  "V-line on a single YAML helmet body line: only that body is bare."
  (lang-markdown-tests--with-yaml-helmet-buffer
    (lang-markdown-tests--with-evil-v-line 2 2
      (should (equal
               '((1 (top-leading . masked) (top-trailing . masked))
                 (2 (body . bare))
                 (3 (body . masked))
                 (4 (bottom-leading . masked) (bottom-trailing . masked)))
               (lang-markdown-tests--fence-variants-by-line))))))

;;; Reveal-interaction with display variant swap

(ert-deftest lang-markdown/gfm-pretty-fences-reveal-aware-display-swap ()
  "When reveal has hidden an overlay's `display' (display=nil and
`gfm-pretty-saved-display' carries the value), the walker updates the
saved-display slot instead of `display' — so the next reveal-restore
picks up the right variant.

Regression: without reveal-awareness, a V-line over a marker line
whose `top-leading' is currently hidden by reveal (cursor on it) would
leave the saved value untouched; on cursor leave, reveal would restore
the masked display even though selection still covered the line."
  (lang-markdown-tests--with-fences-test-buffer
    (let* ((l3-bol (lang-markdown-tests--line-bol 3))
           (tl (cl-find-if
                (lambda (o)
                  (and (eq (overlay-get o 'gfm-pretty-fences-kind) 'top-leading)
                       (= (overlay-start o) l3-bol)))
                (overlays-in l3-bol (1+ l3-bol))))
           (masked (overlay-get tl 'gfm-pretty-display-masked)))
      (should tl)
      ;; Simulate reveal hiding top-leading: stash display in
      ;; saved-display and clear `display'.
      (overlay-put tl 'gfm-pretty-saved-display masked)
      (overlay-put tl 'display nil)
      ;; Activate a V-line over the marker line and run the walker.
      (lang-markdown-tests--with-evil-v-line 3 3
        ;; `display' should remain nil (reveal still hiding it).
        (should-not (overlay-get tl 'display))
        ;; saved-display should now hold the BARE variant so the
        ;; next reveal-restore picks up the selection paint.
        (should (eq (overlay-get tl 'gfm-pretty-display-bare)
                    (overlay-get tl 'gfm-pretty-saved-display)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-border-face-resets-styling ()
  "Border face spec inherits the configured face but resets styling
attrs that would otherwise leak from font-lock onto box edges (slant,
weight, underline, overline, strike-through, box).  Box weight is
pinned to `light' for a hairline frame."
  (let ((spec (gfm-pretty--normalised-border-face 'italic)))
    (should (equal (plist-get spec :inherit) 'italic))
    (should (eq (plist-get spec :slant) 'normal))
    (should (eq (plist-get spec :weight) 'light))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))))

(ert-deftest lang-markdown/gfm-pretty-fences-find-indent-block ()
  (with-temp-buffer
    (insert "Para.\n\n    code one\n    code two\n\nMore.\n")
    (let ((blocks (gfm-pretty-fences--find-indent-blocks nil)))
      (should (= 1 (length blocks)))
      (should (= 4 (nth 2 (car blocks)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-yaml-mode-prefers-treesit ()
  "Helmet language mode picks yaml-ts-mode when grammar available."
  (should (eq 'yaml-ts-mode (gfm-pretty-fences--yaml-mode))))

(ert-deftest lang-markdown/gfm-pretty-fences-yaml-helmet-fontifies-body ()
  "YAML helmet body receives face overlays from the chosen yaml mode."
  (with-temp-buffer
    (insert "---\nkey: value\n---\nbody\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "key")
    (let ((pos (match-beginning 0)))
      (should (cl-some (lambda (ov)
                         (and (overlay-get ov 'gfm-pretty-fences)
                              (overlay-get ov 'face)))
                       (overlays-at pos))))))

(ert-deftest lang-markdown/gfm-pretty-fences-yaml-helmet-empty-body-noerror ()
  "Empty YAML helmet body does not error during rebuild."
  (with-temp-buffer
    (insert "---\n---\nbody\n")
    (should (progn (gfm-pretty-mode 1) t))))

(ert-deftest lang-markdown/gfm-pretty-fences-skip-indent-inside-fence ()
  (with-temp-buffer
    (insert "```\n    looks indented\n```\n")
    (let* ((fenced (gfm-pretty-fences--find-blocks))
           (excluded (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b))) fenced))
           (indents (gfm-pretty-fences--find-indent-blocks excluded)))
      (should (= 0 (length indents))))))

;;; Language → mode mapping

(ert-deftest lang-markdown/gfm-pretty-fences-lang-mode-aliases ()
  "GitHub-recognised language aliases map to canonical major mode symbols.
Cases are restricted to modes that ship with Emacs so the test never skips."
  (dolist (case '(("py"         . python-mode)
                  ("rb"         . ruby-mode)
                  ("cs"         . csharp-mode)
                  ("c#"         . csharp-mode)
                  ("cpp"        . c++-mode)
                  ("javascript" . js-mode)
                  ("jsx"        . js-jsx-mode)
                  ("zsh"        . sh-mode)
                  ("shell"      . sh-mode)
                  ("elisp"      . emacs-lisp-mode)
                  ("emacs-lisp" . emacs-lisp-mode)))
    (should (eq (cdr case) (gfm-pretty-fences--lang-mode (car case))))))

(ert-deftest lang-markdown/gfm-pretty-fences-lang-mode-case-insensitive ()
  "Language tag lookup is case-insensitive."
  (should (eq 'python-mode (gfm-pretty-fences--lang-mode "Python")))
  (should (eq 'csharp-mode (gfm-pretty-fences--lang-mode "C#")))
  (should (eq 'js-mode     (gfm-pretty-fences--lang-mode "JavaScript"))))

(ert-deftest lang-markdown/gfm-pretty-fences-lang-mode-fallback ()
  "Unknown languages fall back to <lang>-mode."
  (should (eq 'totally-made-up-mode
              (gfm-pretty-fences--lang-mode "totally-made-up"))))

(ert-deftest lang-markdown/gfm-pretty-fences-icon-for-aliased-lang ()
  "Icon lookup returns a language-specific glyph (not the fallback)."
  (let ((fallback (nerd-icons-icon-for-mode 'fundamental-mode)))
    (dolist (lang '("py" "rb" "javascript" "zsh" "cs" "cpp"))
      (let ((icon (gfm-pretty-fences--icon-for-lang lang)))
        (should (and (stringp icon) (> (length icon) 0)))
        (should-not (equal icon fallback))))))

;;; Wrap simulation termination guard

(ert-deftest lang-markdown/gfm-pretty-fences-simulate-wrap-zero-width-terminates ()
  "`gfm-pretty--simulate-wrap' returns rather than spinning at width 0."
  (let ((res (with-timeout (1 'timeout)
               (gfm-pretty--simulate-wrap "hello world" 0))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-pretty-fences-simulate-wrap-tiny-width-with-prefix-terminates ()
  "`gfm-pretty--simulate-wrap' terminates when width ≤ cont-prefix-w."
  (let ((res (with-timeout (1 'timeout)
               (gfm-pretty--simulate-wrap "hello world" 1 2))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

;;; Discovery cache

(ert-deftest lang-markdown/gfm-pretty-fences-collect-cache-eq-no-edit ()
  "Two `gfm-pretty--collect' calls on `fences' with no edits return `eq' lists.
Pass 3 moved memoisation into the engine; per-decorator block caches were
retired."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'fences))
           (a (gfm-pretty--collect d))
           (b (gfm-pretty--collect d)))
      (should (eq a b)))))

(ert-deftest lang-markdown/gfm-pretty-fences-collect-cache-invalidates-on-edit ()
  "Edits invalidate the engine `:collect-fn' cache for the fences decorator."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'fences))
           (before (gfm-pretty--collect d)))
      (goto-char (point-max))
      (insert "\n```sh\nfoo\n```\n")
      (let ((after (gfm-pretty--collect d)))
        (should-not (eq before after))))))

(ert-deftest lang-markdown/gfm-pretty-fences-collect-cache-narrowing-resilient ()
  "Engine `:collect-fn' is invoked under widen so narrowing does not hide blocks."
  (with-temp-buffer
    (insert "```bash\nfirst\n```\n\n```sh\nsecond\n```\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'fences))
           (full (gfm-pretty--collect d)))
      (should (cl-some #'identity full))
      ;; Bust the cache and re-collect under narrowing.
      (gfm-pretty--state-set 'fences 'blocks-cache nil)
      (narrow-to-region (point-min) (1+ (point-min)))
      (let ((narrowed (gfm-pretty--collect d)))
        (should (= (length full) (length narrowed)))))))

;;; Per-window display overlays

(ert-deftest lang-markdown/gfm-pretty-fences-per-window-display-overlays ()
  "Buffer in two windows of different widths gets per-window display overlays.
Anchors stay shared across windows; only display overlays carry a
`window' restriction."
  (let ((buf (generate-new-buffer "*gfm-pretty-fences-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "```bash\necho hi\n```\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-pretty-fences))
                                (overlays-in (point-min) (point-max))))
                     (displays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-pretty-fences-display))
                                overlays))
                     (windowed (cl-count-if
                                (lambda (o) (overlay-get o 'window))
                                displays)))
                ;; All display overlays carry a `window' property in 2-window setup.
                (should (> (length displays) 0))
                (should (= (length displays) windowed))))
            (delete-window other)))
      (kill-buffer buf))))

;;; Window-state diff reconciliation

(ert-deftest lang-markdown/gfm-pretty-fences-reconcile-windows-touches-changed-only ()
  "Reconciling windows replaces only the resized window's display overlays."
  (let ((buf (generate-new-buffer "*gfm-pretty-fences-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "```bash\necho hi\n```\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((displays-for
                      (lambda (w)
                        (cl-remove-if-not
                         (lambda (o)
                           (and (overlay-get o 'gfm-pretty-fences-display)
                                (eq (overlay-get o 'window) w)))
                         (gfm-pretty--state-get 'fences 'overlays))))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                ;; Forge a width change for win-a only.
                (gfm-pretty--state-set 'fences 'last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              (gfm-pretty--state-get 'fences 'last-window-state)))
                ;; Drive the deferred rebuild path synchronously by calling
                ;; the per-window helper directly: reconcile schedules
                ;; rebuilds via idle timers, but ERT tests can't easily wait
                ;; for them to fire.  Helper does the same work that the
                ;; idle callback eventually does.
                (gfm-pretty--rebuild-block-for-window (gfm-pretty--get 'fences)
                 (car (gfm-pretty-fences--collect-blocks)) win-a)
                (let ((a-after (funcall displays-for win-a))
                      (b-after (funcall displays-for win-b)))
                  ;; win-a's overlays were replaced.
                  (should-not (cl-intersection a-before a-after))
                  ;; win-b's overlays untouched.
                  (should (= (length b-before) (length b-after)))
                  (should (cl-every (lambda (o) (memq o b-after)) b-before)))))
            (delete-window win-b)))
      (kill-buffer buf))))

;;; Scoped post-edit rebuild

(defun gfm-pretty-fences--test-overlay-set ()
  "Return gfm-pretty-fences overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-pretty-fences)
        (puthash ov t set)))
    set))

(ert-deftest lang-markdown/gfm-pretty-fences-scoped-edit-inside-single-block ()
  "Editing inside one fenced block only rebuilds that block's overlays."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n\n```sh\nfoo\n```\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-fences--collect-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (r1 (gfm-pretty-fences--block-range b1))
           (r2 (gfm-pretty-fences--block-range b2))
           (collect (lambda (range)
                      (cl-remove-if-not
                       (lambda (o) (overlay-get o 'gfm-pretty-fences))
                       (overlays-in (car range) (cdr range)))))
           (b2-before (funcall collect r2)))
      ;; Edit a body line inside b1 only, away from any fence line.
      (save-excursion
        (goto-char (car r1))
        (forward-line 1)
        (let ((p (point)))
          (gfm-pretty--state-set 'fences 'dirty-region
                (cons (1+ p) (1+ p)))))
      (gfm-pretty--scheduled-rebuild)
      (let ((b2-after (funcall collect r2)))
        ;; b2's overlay objects survived `eq'.
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))))))

(ert-deftest lang-markdown/gfm-pretty-fences-scoped-edit-on-fence-boundary-full-rebuild ()
  "Edit overlapping a fence opening line triggers a full rebuild."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-fences--find-blocks))
           (open-beg (nth 0 (car blocks)))
           (open-line-beg (save-excursion
                            (goto-char open-beg) (line-beginning-position)))
           (open-line-end (save-excursion
                            (goto-char open-beg) (line-end-position)))
           (before (gfm-pretty-fences--test-overlay-set)))
      (gfm-pretty--state-set 'fences 'dirty-region (cons open-line-beg open-line-end))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-fences--test-overlay-set)))
        ;; Full rebuild → original overlay objects no longer present.
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-scoped-edit-blank-adjacent-indent-full-rebuild ()
  "Edit on a blank line adjacent to an indent block triggers full rebuild."
  (with-temp-buffer
    (insert "Para.\n\n    code line\n    next code\n\nMore.\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-fences--find-indent-blocks nil))
           (block (car blocks))
           (beg (nth 0 block))
           (blank-beg (save-excursion
                        (goto-char beg) (forward-line -1)
                        (line-beginning-position)))
           (blank-end (save-excursion
                        (goto-char beg) (forward-line -1)
                        (line-end-position)))
           (before (gfm-pretty-fences--test-overlay-set)))
      (gfm-pretty--state-set 'fences 'dirty-region (cons blank-beg blank-end))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-fences--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-pretty-fences-scoped-edit-outside-blocks-noop ()
  "Edit outside every decorated block is a no-op."
  (with-temp-buffer
    (insert "intro line\n\n```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (let ((before (gfm-pretty-fences--test-overlay-set))
          (rebuild-count (plist-get (gfm-pretty--state-get 'fences 'rebuild-stats) :count)))
      (gfm-pretty--state-set 'fences 'dirty-region (cons 1 5))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-fences--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)
        (should (= rebuild-count
                   (plist-get (gfm-pretty--state-get 'fences 'rebuild-stats) :count)))))))

;;; Visible-first prioritisation

(ert-deftest lang-markdown/gfm-pretty-fences-block-visible-p ()
  "`gfm-pretty-fences--block-visible-p' detects overlap with any window range."
  (let ((block (gfm-pretty-fences--make-block
                :kind 'fenced :range (cons 100 200) :payload nil)))
    (should (gfm-pretty-fences--block-visible-p block '((50 . 250))))
    (should (gfm-pretty-fences--block-visible-p block '((130 . 180))))
    (should (gfm-pretty-fences--block-visible-p block '((50 . 100))))
    (should-not (gfm-pretty-fences--block-visible-p block '((1 . 99) (201 . 300))))
    (should-not (gfm-pretty-fences--block-visible-p block nil))
    (should (gfm-pretty-fences--block-visible-p block '((1 . 50) (130 . 180))))))

;;; Performance instrumentation

(ert-deftest lang-markdown/gfm-pretty-fences-stats-increment-across-rebuilds ()
  "Stats accumulate rebuild count and total time across rebuilds."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-pretty-mode 1)
    (let ((before (plist-get (gfm-pretty--state-get 'fences 'rebuild-stats) :count)))
      (gfm-pretty--rebuild (gfm-pretty--get 'fences))
      (let ((after (plist-get (gfm-pretty--state-get 'fences 'rebuild-stats) :count)))
        (should (> after before))))))

(ert-deftest lang-markdown/gfm-pretty-fences-phase-totals-include-required-keys ()
  "Phase totals include the six keys required by the spec."
  (with-temp-buffer
    (insert "---\nk: v\n---\n```bash\necho hi\n```\n\nPara.\n\n    indent\n")
    (gfm-pretty-mode 1)
    (let ((phases (gfm-pretty--state-get 'fences 'phase-totals)))
      (dolist (k '(find-fenced find-yaml find-indent
                   compose-borders compose-overflow apply))
        (should (assq k phases))))))

;;; gfm-pretty-tables tests

(require 'gfm-pretty-tables)

;;; Cell parser

(ert-deftest lang-markdown/gfm-pretty-tables-split-row-simple ()
  (should (equal '("a" "b" "c")
                 (gfm-pretty-tables--split-row "| a | b | c |"))))

(ert-deftest lang-markdown/gfm-pretty-tables-split-row-escaped-pipe ()
  (should (equal '("a | b" "c")
                 (gfm-pretty-tables--split-row "| a \\| b | c |"))))

(ert-deftest lang-markdown/gfm-pretty-tables-split-row-single-tick-code ()
  (should (equal '("a" "`b|c`" "d")
                 (gfm-pretty-tables--split-row "| a | `b|c` | d |"))))

(ert-deftest lang-markdown/gfm-pretty-tables-split-row-double-tick-code ()
  (should (equal '("a" "``b|c``" "d")
                 (gfm-pretty-tables--split-row "| a | ``b|c`` | d |"))))

(ert-deftest lang-markdown/gfm-pretty-tables-split-row-unbalanced-tick ()
  "Unbalanced backtick is treated as literal text."
  (should (equal '("a" "`b" "c")
                 (gfm-pretty-tables--split-row "| a | `b | c |"))))

;;; Block discovery

(ert-deftest lang-markdown/gfm-pretty-tables-find-blocks-standard ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (let ((blocks (gfm-pretty-tables--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest lang-markdown/gfm-pretty-tables-find-blocks-rejects-lone-delim ()
  (with-temp-buffer
    (insert "Some prose.\n| - | - |\nMore prose.\n")
    (should-not (gfm-pretty-tables--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-tables-collect-cache-invalidates-on-edit ()
  "Engine `:collect-fn' for the tables decorator caches by chars-modified tick.
Two calls without an intervening edit return `eq' lists; an edit
invalidates the cache and the next call returns a fresh list reflecting
the new buffer state."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\nintro\n")
    (gfm-pretty-mode 1)
    (let* ((d (gfm-pretty--get 'tables))
           (first (gfm-pretty--collect d))
           (second (gfm-pretty--collect d)))
      (should (eq first second))
      ;; Append a second table; cache must invalidate.
      (goto-char (point-max))
      (insert "\n| C | D |\n| - | - |\n| 3 | 4 |\n")
      (let ((after (gfm-pretty--collect d)))
        (should (= 2 (length after)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-find-blocks-skips-fenced ()
  (with-temp-buffer
    (insert "```\n| A | B |\n| - | - |\n| 1 | 2 |\n```\n")
    (let* ((fenced (gfm-pretty-fences--find-blocks))
           (excluded (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b))) fenced))
           (blocks (gfm-pretty-tables--find-blocks excluded)))
      (should (= 0 (length blocks))))))

;;; Column widths

(ert-deftest lang-markdown/gfm-pretty-tables-column-widths-unaligned ()
  (let* ((rows '(("Header" "B")
                 ("a" "longer")
                 ("xx" "y")))
         (widths (gfm-pretty-tables--column-widths rows)))
    (should (equal 6 (aref widths 0)))
    (should (equal 6 (aref widths 1)))))

(ert-deftest lang-markdown/gfm-pretty-tables-box-width ()
  ;; 2 cols, widths 3 and 5 → 2 + (5) + (7) + 1 = 15
  (should (= 15 (gfm-pretty-tables--box-width (vector 3 5)))))

;;; Cell wrapping

(ert-deftest lang-markdown/gfm-pretty-tables-cell-tokens-splits-on-whitespace ()
  (should (equal '("foo" "bar" "baz")
                 (gfm-pretty-tables--cell-tokens "  foo  bar baz "))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-tokens-empty-string ()
  (should (equal '() (gfm-pretty-tables--cell-tokens "")))
  (should (equal '() (gfm-pretty-tables--cell-tokens "   "))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-tokens-preserves-properties ()
  (let* ((src (concat "abc " (propertize "def" 'face 'bold)))
         (tokens (gfm-pretty-tables--cell-tokens src)))
    (should (equal '("abc" "def") tokens))
    (should (eq 'bold (get-text-property 0 'face (cadr tokens))))))

(ert-deftest lang-markdown/gfm-pretty-tables-slice-by-visible-width-basic ()
  (should (equal '("abc" "de") (gfm-pretty-tables--slice-by-visible-width "abcde" 3))))

(ert-deftest lang-markdown/gfm-pretty-tables-slice-by-visible-width-zero-falls-back ()
  ;; Width 0 must still make progress (one char per slice).
  (let ((chunks (gfm-pretty-tables--slice-by-visible-width "ab" 0)))
    (should (= 2 (length chunks)))))

(ert-deftest lang-markdown/gfm-pretty-tables-wrap-cell-no-wrap ()
  (should (equal '("hello") (gfm-pretty-tables--wrap-cell "hello" 10))))

(ert-deftest lang-markdown/gfm-pretty-tables-wrap-cell-word-boundary ()
  (should (equal '("the quick" "brown fox")
                 (gfm-pretty-tables--wrap-cell "the quick brown fox" 9))))

(ert-deftest lang-markdown/gfm-pretty-tables-wrap-cell-hard-break-long-word ()
  (let ((lines (gfm-pretty-tables--wrap-cell "abcdefghij" 4)))
    (should (cl-every (lambda (l) (<= (string-width l) 4)) lines))
    (should (equal "abcdefghij" (apply #'concat lines)))))

(ert-deftest lang-markdown/gfm-pretty-tables-wrap-cell-empty-returns-one-empty-line ()
  (should (equal '("") (gfm-pretty-tables--wrap-cell "" 5))))

(ert-deftest lang-markdown/gfm-pretty-tables-wrap-cell-preserves-properties ()
  (let* ((src (concat (propertize "abc" 'face 'bold) " def"))
         (lines (gfm-pretty-tables--wrap-cell src 3)))
    (should (eq 'bold (get-text-property 0 'face (car lines))))))

;;; Width fitting

(ert-deftest lang-markdown/gfm-pretty-tables-fit-widths-under-budget-passthrough ()
  (should (equal (vector 3 5) (gfm-pretty-tables--fit-widths (vector 3 5) 100))))

(ert-deftest lang-markdown/gfm-pretty-tables-fit-widths-caps-widest ()
  ;; natural sum 30, budget 20: smaller col fits naturally, widest capped.
  (let ((fitted (gfm-pretty-tables--fit-widths (vector 5 25) 20)))
    (should (= 5 (aref fitted 0)))
    (should (= 15 (aref fitted 1)))))

(ert-deftest lang-markdown/gfm-pretty-tables-fit-widths-equal-distribution ()
  ;; Equal natural widths over budget → all capped equally.
  (let ((fitted (gfm-pretty-tables--fit-widths (vector 20 20) 30)))
    (should (= (aref fitted 0) (aref fitted 1)))
    (should (<= (+ (aref fitted 0) (aref fitted 1)) 30))))

(ert-deftest lang-markdown/gfm-pretty-tables-fit-widths-uses-full-budget ()
  "Integer slack from binary search is distributed so sum = budget."
  ;; Natural [15 118 57], budget 50: water cap is 17 → 15+17+17=49.
  ;; The 1 unit of slack must be redistributed so total = 50.
  (let ((fitted (gfm-pretty-tables--fit-widths (vector 15 118 57) 50)))
    (should (= 50 (cl-loop for w across fitted sum w)))))

(ert-deftest lang-markdown/gfm-pretty-tables-fit-widths-floor-at-1 ()
  ;; Tiny budget shouldn't produce zero widths.
  (let ((fitted (gfm-pretty-tables--fit-widths (vector 10 10 10) 1)))
    (should (cl-every (lambda (w) (>= w 1)) (cl-coerce fitted 'list)))))

;;; Multi-line compose

(ert-deftest lang-markdown/gfm-pretty-tables-compose-multiline-row-single-line ()
  "If no cell exceeds its width, output is a single line (no `\\n')."
  (let ((row (gfm-pretty-tables--compose-multiline-row '("a" "b") (vector 1 1)
                                                'body-default)))
    (should-not (string-match-p "\n" row))))

(ert-deftest lang-markdown/gfm-pretty-tables-compose-multiline-row-wraps-cell ()
  "A cell wider than its column wraps to multiple visual lines."
  (let* ((row (gfm-pretty-tables--compose-multiline-row
               '("a" "one two three four") (vector 1 9)
               'body-default))
         (lines (split-string row "\n")))
    (should (>= (length lines) 2))
    (should (cl-every (lambda (l) (= (length l) (length (car lines)))) lines))))

(ert-deftest lang-markdown/gfm-pretty-tables-compose-multiline-row-pads-short-cells ()
  "Short cells get padded with blank lines so columns stay aligned."
  (let* ((row (gfm-pretty-tables--compose-multiline-row
               '("x" "long content that wraps") (vector 1 6)
               'body-default))
         (lines (split-string row "\n")))
    ;; All lines have equal display width.
    (should (cl-every (lambda (l) (= (length l) (length (car lines)))) lines))
    ;; First line carries `x' but later lines have only spaces / decoration in
    ;; the first column.
    (should (string-match-p "x" (car lines)))
    (dolist (l (cdr lines))
      (should-not (string-match-p "x" (substring l 0 3))))))

;;; Cell fontification

(ert-deftest lang-markdown/gfm-pretty-tables-fontify-cell-applies-bold-face ()
  "Bold markdown inside a cell receives `markdown-bold-face'."
  (let ((s (gfm-pretty-tables--fontify-cell "**bold**")))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face s)))
                         (or (eq f 'markdown-bold-face)
                             (and (listp f) (memq 'markdown-bold-face f)))))
                     (number-sequence 0 (1- (length s)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-fontify-cell-applies-code-face ()
  "Inline code inside a cell receives `markdown-inline-code-face'."
  (let ((s (gfm-pretty-tables--fontify-cell "`code`")))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face s)))
                         (or (eq f 'markdown-inline-code-face)
                             (and (listp f)
                                  (memq 'markdown-inline-code-face f)))))
                     (number-sequence 0 (1- (length s)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-fontify-cell-preserves-width ()
  "Fontified cell has the same visible width as the raw cell when
`markdown-hide-markup' is off — i.e. markup chars remain on screen."
  (with-temp-buffer
    (setq buffer-invisibility-spec '(t))
    (dolist (raw '("plain" "**bold**" "*it*" "`code`" "[t](u)" ""))
      (let ((fontified (gfm-pretty-tables--fontify-cell raw)))
        (should (= (string-width raw)
                   (gfm-pretty-tables--visible-width fontified)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-honours-display ()
  "`display' string property changes visible width."
  (let ((s (concat "abc" (propertize "X" 'display "longer") "de")))
    (should (= (+ 5 (string-width "longer"))
               (gfm-pretty-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-honours-invisibility-spec ()
  "Invisible-tagged chars only shrink width when in `buffer-invisibility-spec'."
  (let ((s (concat "ab" (propertize "XX" 'invisible 'tag) "cd")))
    (with-temp-buffer
      (setq buffer-invisibility-spec nil)
      (should (= 6 (gfm-pretty-tables--visible-width s)))
      (setq buffer-invisibility-spec '(tag))
      (should (= 4 (gfm-pretty-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-honours-composition ()
  "`composition' property compresses visible width to that of the composed glyph.
This is what `markdown-mode' uses to hide URLs when `markdown-hide-urls'
is non-nil — `(url)' is composed into a single chain glyph."
  (let ((s (copy-sequence "abcXXXXXde")))
    (compose-string s 3 8 ?Y)
    (should (= 6 (gfm-pretty-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-link-with-hidden-url ()
  "Fontified link cell with `markdown-hide-urls' on reports the visible-only width."
  (let ((prev (default-value 'markdown-hide-urls)))
    (unwind-protect
        (progn
          (setq-default markdown-hide-urls t)
          ;; Drop any cached fontify buffer so the new default takes effect.
          (when (get-buffer " *gfm-pretty-tables-fontify*")
            (kill-buffer " *gfm-pretty-tables-fontify*"))
          (let ((s (gfm-pretty-tables--fontify-cell
                    "[label](https://example.com/very/long/path)")))
            (should (<= (gfm-pretty-tables--visible-width s)
                        (+ (length "label") 3)))))
      (setq-default markdown-hide-urls prev)
      (when (get-buffer " *gfm-pretty-tables-fontify*")
        (kill-buffer " *gfm-pretty-tables-fontify*")))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-compute-walks-source-overlays ()
  "With (buffer beg end) args, the walker honours overlay display strings.
An overlay carrying a `display' string in the source buffer is counted
at the display width, not the underlying text width."
  (with-temp-buffer
    (insert "hello world")
    (let ((ov (make-overlay 1 6)))           ; covers "hello"
      (overlay-put ov 'display "X"))
    ;; Region [1, 12) is "hello world": "hello" -> "X" (1) + " world" (6) = 7.
    (should (= 7 (gfm-pretty-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-compute-honours-source-invisible ()
  "With source args, an overlay `invisible' prop in `buffer-invisibility-spec'
shrinks the measured width."
  (with-temp-buffer
    (insert "abcdef")
    (setq buffer-invisibility-spec '(tag))
    (let ((ov (make-overlay 3 5)))           ; covers "cd"
      (overlay-put ov 'invisible 'tag))
    (should (= 4 (gfm-pretty-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-compute-region-trims-padding ()
  "Source-region measurement trims leading/trailing whitespace of the cell."
  (with-temp-buffer
    (insert "  abc  ")
    (should (= 3 (gfm-pretty-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-nil-region-args-unchanged ()
  "Passing nil for the new region args leaves the string-walk path unchanged."
  (let ((s (concat "abc" (propertize "X" 'display "longer") "de")))
    (should (= (gfm-pretty-tables--visible-width s)
               (gfm-pretty-tables--visible-width s nil nil nil)))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-link-pos-finds-inline-link ()
  "Inline link inside a cell is locatable from the row overlay."
  (with-temp-buffer
    (gfm-mode)
    (insert "| col |\n| --- |\n| [text](https://example.com) |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (let* ((row-ov (cl-find-if (lambda (o)
                                 (overlay-get o 'gfm-pretty-tables-cell-bounds))
                               (overlays-at (line-beginning-position))))
           (pos (gfm-pretty-tables--cell-link-pos row-ov 0)))
      (should pos)
      (save-excursion
        (goto-char pos)
        (should (looking-at-p "\\[text\\](https://example\\.com)"))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-link-pos-no-link-returns-nil ()
  "A cell with no link returns nil from `cell-link-pos'."
  (with-temp-buffer
    (gfm-mode)
    (insert "| col |\n| --- |\n| plain text |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (let ((row-ov (cl-find-if (lambda (o)
                                (overlay-get o 'gfm-pretty-tables-cell-bounds))
                              (overlays-at (line-beginning-position)))))
      (should-not (gfm-pretty-tables--cell-link-pos row-ov 0)))))

(ert-deftest lang-markdown/gfm-pretty-tables-row-char-bounds-aligns-after-composition ()
  "Cell char bounds reflect actual cell string length, not visible width.
A cell with a composition (long source, narrow display) padded to its
column width must still cover all its source chars in the bounds."
  (let* ((cell (copy-sequence "abcXXXXXde"))
         (_ (compose-string cell 3 8 ?Y))
         (bounds (gfm-pretty-tables--row-char-bounds (list cell "ok") (vector 6 2))))
    ;; cell 0: 1 (after pipe) → 1 + 2 + 10 (raw len) + 0 (pad) = 13.
    (should (equal (nth 0 bounds) (cons 1 13)))
    ;; gap at 13. cell 1: 14 → 14 + 2 + 2 + 0 = 18.
    (should (equal (nth 1 bounds) (cons 14 18)))))

(ert-deftest lang-markdown/gfm-pretty-tables-row-char-bounds-end-matches-compose-row-length ()
  "Last cell's end + 1 (closing pipe) equals `compose-row's string length."
  (let* ((cells '("ab" "cd" "e"))
         (col-widths (vector 2 3 1))
         (s (gfm-pretty-tables--compose-row cells col-widths 'body-default))
         (bounds (gfm-pretty-tables--row-char-bounds cells col-widths)))
    (should (= (length s) (1+ (cdr (car (last bounds))))))))

(ert-deftest lang-markdown/gfm-pretty-tables-multiline-row-char-bounds-per-line ()
  "Per-visual-line bounds reflect the wrapped cell content on each line."
  (let* ((cells '("a" "one two three"))
         (col-widths (vector 1 5))
         (per-line (gfm-pretty-tables--multiline-row-char-bounds cells col-widths)))
    ;; Two visual lines (cell 1 wraps).
    (should (>= (length per-line) 2))
    ;; Each line's bounds list has one entry per column.
    (dolist (cb per-line)
      (should (= (length cb) 2)))))

(ert-deftest lang-markdown/gfm-pretty-tables-compose-row-preserves-cell-faces ()
  "`compose-row' on body-alt row keeps existing markdown faces on cell text."
  (let* ((cell (gfm-pretty-tables--fontify-cell "**bold**"))
         (row (gfm-pretty-tables--compose-row (list cell) (vector 8) 'body-alt)))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face row)))
                         (and (listp f) (memq 'markdown-bold-face f))))
                     (number-sequence 0 (1- (length row)))))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face row)))
                         (and (listp f)
                              (memq 'gfm-pretty-tables-row-alt-face f))))
                     (number-sequence 0 (1- (length row)))))))

;;; Overlay lifetime

(ert-deftest lang-markdown/gfm-pretty-tables-overlays-not-evaporative ()
  "Table overlays must not evaporate when their region empties."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-pretty-tables)
        (should-not (overlay-get ov 'evaporate))))))

;;; Indirect editing

(ert-deftest lang-markdown/gfm-pretty-tables-block-at-point-inside ()
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (search-forward "1")
    (let ((bounds (gfm-pretty-tables--block-at-point)))
      (should bounds)
      (should (< (car bounds) (point)))
      (should (> (cdr bounds) (point))))))

(ert-deftest lang-markdown/gfm-pretty-tables-block-at-point-outside-returns-nil ()
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (should-not (gfm-pretty-tables--block-at-point))))

(ert-deftest lang-markdown/gfm-pretty-tables-edit-table-command-defined ()
  (should (commandp 'gfm-pretty-tables-edit-table-at-point)))

(ert-deftest lang-markdown/gfm-pretty-tables-edit-cell-command-defined ()
  (should (commandp 'gfm-pretty-tables-edit-cell-at-point)))

(ert-deftest lang-markdown/gfm-pretty-tables-edit-cell-no-spurious-newline ()
  "Committing a cell edit with no changes must not split the row.
`markdown--edit-indirect-after-commit-function' appends \\n to the
committed region, treating it as a code block.  For a cell edit the
region is just cell content; the trailing newline must be stripped."
  (with-temp-buffer
    (gfm-mode)
    (insert "| A             | B   |\n| ------------- | --- |\n"
            "| `(parameter)` | foo |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (forward-char 4)
    (let* ((info (gfm-pretty-tables--cell-info-at-point))
           (row-ov (car info))
           (idx (cdr info))
           (bounds (gfm-pretty-tables--cell-content-bounds row-ov idx))
           (before-line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
           (src-buf (current-buffer))
           (edit-indirect-guess-mode-function
            (lambda (_p _b _e) (markdown-mode)))
           (buf (edit-indirect-region (car bounds) (cdr bounds) nil)))
      (with-current-buffer buf
        (setq-local require-final-newline nil)
        (setq-local mode-require-final-newline nil)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-pretty-tables--cell-edit-mark-pending nil t)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-pretty-tables--cell-edit-sanitise nil t))
      (with-current-buffer src-buf
        (add-hook 'edit-indirect-after-commit-functions
                  #'gfm-pretty-tables--cell-edit-after-commit
                  'append 'local))
      (with-current-buffer buf (edit-indirect-commit))
      (with-current-buffer src-buf
        (goto-char (point-min))
        (forward-line 2)
        (let ((after-line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
          (should (equal before-line after-line)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-edit-sanitise-strips-newlines ()
  (with-temp-buffer
    (insert "a\nb\nc")
    (gfm-pretty-tables--cell-edit-sanitise)
    (should (equal "a b c" (buffer-string)))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-edit-sanitise-escapes-pipe ()
  (with-temp-buffer
    (insert "a|b")
    (gfm-pretty-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-edit-sanitise-keeps-existing-escape ()
  (with-temp-buffer
    (insert "a\\|b")
    (gfm-pretty-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-edit-sanitise-leading-pipe ()
  (with-temp-buffer
    (insert "|a")
    (gfm-pretty-tables--cell-edit-sanitise)
    (should (equal "\\|a" (buffer-string)))))

;;; Cell bounds + active-cell highlight

(ert-deftest lang-markdown/gfm-pretty-tables-cell-bounds-simple ()
  (with-temp-buffer
    (insert "| A | B |")
    (let ((cb (gfm-pretty-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb)))
      ;; First cell content begins right after the leading `|'.
      (should (eq ?| (char-before (car (nth 0 cb)))))
      (should (eq ?| (char-after  (cdr (nth 0 cb))))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-bounds-honours-escape ()
  (with-temp-buffer
    (insert "| a \\| b | c |")
    (let ((cb (gfm-pretty-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-info-at-point ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (let ((info (gfm-pretty-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info))))))

(ert-deftest lang-markdown/gfm-pretty-tables-active-cell-highlight-applied ()
  "Display string carries the active-cell face after entering the row."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (gfm-pretty-tables--update-cursor-highlight)
    (let* ((ov gfm-pretty-tables--highlighted-row-ov)
           (disp (overlay-get ov 'display))
           (has-face (cl-some
                      (lambda (i)
                        (let ((f (get-text-property i 'face disp)))
                          (or (eq f 'gfm-pretty-tables-active-cell-face)
                              (and (listp f)
                                   (memq 'gfm-pretty-tables-active-cell-face f)))))
                      (number-sequence 0 (1- (length disp))))))
      (should has-face)
      (should gfm-pretty-tables--cursor-anchor))))

(ert-deftest lang-markdown/gfm-pretty-tables-cursor-highlight-restores-off-row ()
  "Moving point out of a table restores the cursor and original display."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\nout of table\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (gfm-pretty-tables--update-cursor-highlight)
    (let ((row-ov gfm-pretty-tables--highlighted-row-ov))
      (goto-char (point-max))
      (gfm-pretty-tables--update-cursor-highlight)
      (should-not gfm-pretty-tables--highlighted-row-ov)
      (should-not (overlay-get row-ov 'gfm-pretty-saved-display))
      (should-not gfm-pretty-tables--cursor-anchor))))

;;; Header column reordering

(ert-deftest lang-markdown/gfm-pretty-tables-swap-column-right-swaps ()
  (with-temp-buffer
    (insert "| A | B | C |\n| - | - | - |\n| 1 | 2 | 3 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (gfm-pretty-tables-swap-column-right)
    (goto-char (point-min))
    (let ((header-line (buffer-substring-no-properties
                        (point) (line-end-position)))
          (body-line (progn (forward-line 2)
                            (buffer-substring-no-properties
                             (point) (line-end-position)))))
      (should (string-match-p "B.*A" header-line))
      (should (string-match-p "2.*1" body-line)))))

(ert-deftest lang-markdown/gfm-pretty-tables-swap-column-left-edge-noop ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (let ((before (buffer-string)))
      (gfm-pretty-tables-swap-column-left)
      (should (equal (buffer-string) before)))))

(ert-deftest lang-markdown/gfm-pretty-tables-swap-column-on-body-noop ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (let ((before (buffer-string)))
      (gfm-pretty-tables-swap-column-right)
      (should (equal (buffer-string) before)))))

;;; Cell-wise navigation

(ert-deftest lang-markdown/gfm-pretty-tables-cell-forward-moves-cell ()
  (with-temp-buffer
    (insert "| A | B | C |\n| - | - | - |\n| 1 | 2 | 3 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point))) ; stand on the digit
    (let ((before (gfm-pretty-tables--cell-info-at-point)))
      (should before)
      (gfm-pretty-tables-cell-forward)
      (let ((after (gfm-pretty-tables--cell-info-at-point)))
        (should after)
        (should (= (1+ (cdr before)) (cdr after)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-tab-wraps-to-next-row ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "B")
    (goto-char (1- (point)))
    (gfm-pretty-tables-cell-tab)
    (let ((info (gfm-pretty-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info)))
      ;; Landed on body row 1 (`1' digit nearby).
      (should (string-match-p "1" (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-tab-inserts-row-at-end ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "2")
    (goto-char (1- (point)))
    (gfm-pretty-tables-cell-tab)
    (let ((info (gfm-pretty-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info)))
      ;; New body row is empty: source line of the form `|  |  |'.
      (should (string-match-p "^|[[:space:]]*|[[:space:]]*|"
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-cell-backtab-wraps-to-prev-row ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (gfm-pretty-tables-cell-backtab)
    (let ((info (gfm-pretty-tables--cell-info-at-point)))
      (should info)
      ;; Wrapped to header row's last cell.
      (should (= 1 (cdr info)))
      (should (string-match-p "B" (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-row-down-skips-delim ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (gfm-pretty-tables-row-down)
    ;; The body row's source line begins with `|', followed by ` 1 ' for cell 0.
    ;; row-down lands on the first content char, which is the space right after `|'.
    (should (string-match-p "^| 1 "
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))
    (should (eq ?\s (char-after (point))))))

(ert-deftest lang-markdown/gfm-pretty-tables-row-down-stops-at-table-edge ()
  "Moving down from the last row of a table does not jump into the next table."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "Some prose here.\n\n"
            "| C | D |\n| - | - |\n| 9 | 8 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (should (gfm-pretty-tables--cell-info-at-point))
    (should-not (gfm-pretty-tables--row-on-relative-line 1))))

(ert-deftest lang-markdown/gfm-pretty-tables-row-up-stops-at-table-edge ()
  "Moving up from the first body row of a table stays at the header row."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "Prose.\n\n"
            "| C | D |\n| - | - |\n| 9 | 8 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "9")
    (goto-char (1- (point)))
    (should (gfm-pretty-tables--cell-info-at-point))
    (let ((up (gfm-pretty-tables--row-on-relative-line -1)))
      ;; The only row above 9 inside this block is its header row.
      (should up)
      (let ((header-line
             (save-excursion (goto-char (overlay-start up))
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))
        (should (string-match-p "C" header-line))))))

(ert-deftest lang-markdown/gfm-pretty-tables-snap-from-non-cell-point ()
  "Snap moves a non-cell point on a row line into the first cell."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position)) ; on `|', not in any cell
    (let ((before (point)))
      (gfm-pretty-tables--maybe-snap-to-cell)
      (should (> (point) before))
      (let* ((info (gfm-pretty-tables--cell-info-at-point))
             (cb (overlay-get (car info) 'gfm-pretty-tables-cell-bounds))
             (cell0 (nth 0 cb)))
        (should (>= (point) (car cell0)))
        (should (< (point) (cdr cell0)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-snap-noop-when-in-cell ()
  "Snap leaves point alone when already inside a cell range."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "2")
    (goto-char (1- (point)))
    (let ((before (point)))
      (gfm-pretty-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-pretty-tables-snap-skips-invisible-row ()
  "Snap is a no-op when the row line is invisible."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (let ((lbeg (line-beginning-position))
          (lend (line-end-position)))
      (put-text-property lbeg lend 'invisible t)
      (add-to-invisibility-spec t)
      (goto-char lbeg)
      (let ((before (point)))
        (gfm-pretty-tables--maybe-snap-to-cell)
        (should (= before (point)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-snap-skipped-during-isearch ()
  "Snap is a no-op while `isearch-mode' is active so isearch can park
point on a column gap or border without being yanked back to cell 0."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position)) ; on `|', not in any cell
    (let ((before (point))
          (isearch-mode " Isearch"))
      (gfm-pretty-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-pretty-tables-snap-skipped-for-search-commands ()
  "Snap is a no-op when `this-command' is a search-style command, so
evil-search-next and friends don't have point yanked back."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position))
    (let ((before (point))
          (this-command 'evil-search-next))
      (gfm-pretty-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-pretty-tables-isearch-advances-when-match-ends-on-pipe ()
  "The post-command cursor-highlight hook must not rewind point when a
search lands point on a `|' (outside cell bounds).  Without the snap
suppression, the hook moves point back to cell 0 and the next
`re-search-forward' finds the same match — an infinite loop."
  (with-temp-buffer
    (insert "| col1 | col2 |\n")
    (insert "|------|------|\n")
    (insert "| foo | bar |\n")
    (insert "| foo | qux |\n")
    (markdown-mode)
    (gfm-pretty-mode 1)
    (let ((gfm-pretty--rebuild-timer nil)) (gfm-pretty-tables--rebuild))
    (goto-char (point-min))
    ;; Simulate the body of isearch's command loop: search, then run
    ;; post-command-hook with `isearch-mode' active.  Match-end of
    ;; "foo " lands on a `|' (outside cell bounds) — the snap, if not
    ;; suppressed, would rewind point and trap the next search.
    (let ((isearch-mode " Isearch")
          points)
      (should (re-search-forward "foo " nil t))
      (gfm-pretty-tables--update-cursor-highlight)
      (push (point) points)
      (should (re-search-forward "foo " nil t))
      (gfm-pretty-tables--update-cursor-highlight)
      (push (point) points)
      (setq points (nreverse points))
      (should (< (nth 0 points) (nth 1 points))))))

;;; Evil shim

(ert-deftest lang-markdown/gfm-pretty-tables-evil-edit-commands-listed ()
  (should (boundp 'gfm-pretty-tables--evil-edit-commands))
  (should (memq 'evil-insert gfm-pretty-tables--evil-edit-commands))
  (should (memq 'evil-change gfm-pretty-tables--evil-edit-commands))
  (should (memq 'evil-open-below gfm-pretty-tables--evil-edit-commands)))

;;; Mode lifecycle

(ert-deftest lang-markdown/gfm-pretty-tables-mode-creates-overlays ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-tables))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-tables-mode-removes-overlays ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (gfm-pretty-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-pretty-tables))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-tables-enabled-via-gfm-mode-hook ()
  (should (memq 'gfm-pretty-mode gfm-mode-hook)))

;;; Narrowing-resilient discovery and teardown — tables

(defun lang-markdown-tests--two-slide-tables-buffer ()
  "Insert a two-slide buffer with one table per slide; return widened mark."
  (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
  (insert "\n# slide 2\n\n")
  (insert "| C | D |\n| - | - |\n| 3 | 4 |\n"))

(ert-deftest lang-markdown/gfm-pretty-tables-narrowed-rebuild-does-not-signal ()
  "Narrowed rebuild over a widened cache must not raise `args-out-of-range'.
Pre-fix, `gfm-pretty-tables--apply-table' touched header positions outside the
narrowing and signalled.  See fix-gfm-narrowing-safety."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-pretty-tables--rebuild) t)))))

(ert-deftest lang-markdown/gfm-pretty-tables-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count.
Pre-fix the full-clear `--remove-overlays' under narrowing left
off-narrowing overlays untracked."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-pretty-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty-tables--rebuild)
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-pretty-tables))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length (gfm-pretty--state-get 'tables 'overlays)) on-buffer))))))

(defun lang-markdown-tests--tables-display-strings-by-pos ()
  "Return ((BEG . DISPLAY-STRING) …) for tables display overlays, sorted."
  (mapcar (lambda (o) (cons (overlay-start o) (overlay-get o 'display)))
          (cl-sort
           (cl-remove-if-not
            (lambda (o) (overlay-get o 'gfm-pretty-tables-display))
            (overlays-in (point-min) (point-max)))
           #'< :key #'overlay-start)))

(ert-deftest lang-markdown/gfm-pretty-tables-per-window-rebuild-converges ()
  "Repeated `--rebuild-block-for-window' produces identical display strings.
Pre-fix the per-window path parsed the table before removing the
prior render's display overlays, so `--transcribe-source-overlays'
spliced its own output back into every cell and the second pass
diverged from (and grew larger than) the first."
  (let ((buf (generate-new-buffer " *gfm-pretty-tables-converge*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| Workflow | Schedule | Status | Last |\n")
            (insert "| -------- | -------- | ------ | ---- |\n")
            (insert "| onboard  | daily    | green  | 9am  |\n"))
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (gfm-pretty-mode 1)
            (let* ((blocks (gfm-pretty-tables--find-blocks
                            (gfm-pretty-tables--fenced-ranges)))
                   (block (car blocks))
                   (win (selected-window)))
              (gfm-pretty-tables--rebuild-block-for-window block win)
              (let ((first (lang-markdown-tests--tables-display-strings-by-pos)))
                (gfm-pretty-tables--rebuild-block-for-window block win)
                (let ((second (lang-markdown-tests--tables-display-strings-by-pos)))
                  (should (equal first second))
                  (should-not (cl-some
                               (lambda (entry)
                                 (let ((d (cdr entry)))
                                   (and (stringp d)
                                        (string-match-p
                                         "│ │ │ │ │ " d))))
                               second)))))))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-tables-narrowed-per-window-rebuild-matches-fresh ()
  "Narrowed `--reconcile-windows' then widen + reconcile yields fresh-widened display strings.
Drives the presentation-shape sequence
\(narrow → reconcile-windows → widen → reconcile-windows): pre-fix
the per-window rebuild path spliced its own prior render into every
cell, so display strings diverged from a clean full rebuild."
  :tags '(narrowing-regression)
  (let ((buf (generate-new-buffer " *gfm-pretty-tables-narrow-converge*"))
        baseline after)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (lang-markdown-tests--two-slide-tables-buffer))
          (set-window-buffer (selected-window) buf)
          (with-current-buffer buf
            (gfm-pretty-mode 1)
            (gfm-pretty-tables--rebuild)
            (setq baseline (lang-markdown-tests--tables-display-strings-by-pos))
            (let ((slide-1-end (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "# slide 2")
                                 (line-beginning-position))))
              (narrow-to-region (point-min) slide-1-end))
            (cl-flet ((forge-resize ()
                        (gfm-pretty--state-set
                         'tables 'last-window-state
                         (mapcar (lambda (e) (cons (car e) (1- (cdr e))))
                                 (gfm-pretty--state-get
                                  'tables 'last-window-state)))))
              (forge-resize)
              (gfm-pretty-tables--reconcile-windows)
              (widen)
              (forge-resize)
              (gfm-pretty-tables--reconcile-windows))
            (setq after (lang-markdown-tests--tables-display-strings-by-pos))
            (should (equal baseline after))))
      (kill-buffer buf))))

;;; Per-rebuild width cache

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-fast-path-matches-uncached ()
  "Fast path returns same width as full computation for plain strings."
  (let ((s "hello world"))
    (let ((gfm-pretty-tables--width-cache nil))
      (should (= (string-width s) (gfm-pretty-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-honours-display-prop ()
  "Cached width matches uncached for cells with `display' replacements."
  (let* ((s (concat "abc" (propertize "X" 'display "yyy") "de"))
         (uncached (let ((gfm-pretty-tables--width-cache nil))
                     (gfm-pretty-tables--visible-width s)))
         (cache (make-hash-table :test 'eq)))
    (let ((gfm-pretty-tables--width-cache cache))
      (should (= uncached (gfm-pretty-tables--visible-width s)))
      ;; Second call hits the cache.
      (should (= uncached (gfm-pretty-tables--visible-width s)))
      (should (gethash s cache)))))

(ert-deftest lang-markdown/gfm-pretty-tables-visible-width-ignores-auto-compositions ()
  "Auto-compositions (e.g. ligatures) must not shrink visible-width.
Overlay display strings do not run `composition-function-table', so
counting an auto-composition would under-pad the cell."
  (let ((s (concat (propertize "x" 'invisible 'gfm-test)
                   "fl"))
        (buffer-invisibility-spec '((gfm-test . t))))
    ;; Even if `find-composition' would report a composition for "fl"
    ;; in the current buffer, the cell string carries no `composition'
    ;; text-property, so width must equal the underlying char widths.
    (should (= 2 (gfm-pretty-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-honours-composition-prop ()
  "Cached width matches uncached for cells with `composition' property."
  (let ((s (copy-sequence "abcXXXXXde")))
    (compose-string s 3 8 ?Y)
    (let* ((uncached (let ((gfm-pretty-tables--width-cache nil))
                       (gfm-pretty-tables--visible-width s)))
           (cache (make-hash-table :test 'eq))
           (gfm-pretty-tables--width-cache cache))
      (should (= uncached (gfm-pretty-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-honours-invisible-prop ()
  "Cached width matches uncached for cells with hidden `invisible' segments."
  (let* ((s (concat "abc" (propertize "HIDE" 'invisible 'gfm-test) "de"))
         (buffer-invisibility-spec '((gfm-test . t)))
         (uncached (let ((gfm-pretty-tables--width-cache nil))
                     (gfm-pretty-tables--visible-width s)))
         (cache (make-hash-table :test 'eq))
         (gfm-pretty-tables--width-cache cache))
    (should (= 5 uncached))
    (should (= uncached (gfm-pretty-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-not-shared-across-eq-distinct-strings ()
  "Cache is `eq'-keyed; two equal-but-distinct strings do not share entries."
  (let* ((cache (make-hash-table :test 'eq))
         (gfm-pretty-tables--width-cache cache)
         (a (copy-sequence "hello"))
         (b (copy-sequence "hello")))
    (gfm-pretty-tables--visible-width a)
    (should (gethash a cache))
    (should-not (gethash b cache))))

(ert-deftest lang-markdown/gfm-pretty-tables-width-cache-substring-not-stale ()
  "A substring of a fontified cell is measured fresh, not from the parent's hit."
  (let* ((parent (gfm-pretty-tables--fontify-cell "hello world"))
         (cache (make-hash-table :test 'eq))
         (gfm-pretty-tables--width-cache cache)
         (parent-w (gfm-pretty-tables--visible-width parent))
         (sub (substring parent 0 5))
         (sub-w (gfm-pretty-tables--visible-width sub)))
    (should (= parent-w (gfm-pretty-tables--visible-width parent)))
    (should (= sub-w (gfm-pretty-tables--visible-width sub)))
    (should (= 5 sub-w))
    (should-not (= parent-w sub-w))))

;;; Shared row layout

(ert-deftest lang-markdown/gfm-pretty-tables-row-layout-feeds-both-helpers ()
  "Layout-based helpers produce strings/bounds equal to the legacy callers.
Verifies the packed bounds vector matches the legacy nested-list shape
emitted by `gfm-pretty-tables--multiline-row-char-bounds'."
  (dolist (role '(header body-default body-alt))
    (let* ((cells '("alpha" "beta gamma" "d"))
           (col-widths (vector 5 4 1))
           (layout (gfm-pretty-tables--row-layout cells col-widths))
           (composed-direct (gfm-pretty-tables--compose-multiline-row
                             cells col-widths role))
           (composed-via-layout (gfm-pretty-tables--compose-row-from-layout
                                 layout col-widths role))
           (bounds-direct (gfm-pretty-tables--multiline-row-char-bounds
                           cells col-widths))
           (vec (gfm-pretty-tables--row-layout-bounds-vec layout))
           (n-cells (gfm-pretty-tables--row-layout-n-cells layout)))
      (should (equal composed-direct composed-via-layout))
      (cl-loop for line-cb in bounds-direct
               for line from 0
               do (cl-loop for cb in line-cb
                           for cell from 0
                           for base = (* 2 (+ (* line n-cells) cell))
                           do (should (= (car cb) (aref vec base)))
                              (should (= (cdr cb) (aref vec (1+ base)))))))))

;;; Scoped rebuild

(defun gfm-pretty-tables--test-overlay-set ()
  "Return the gfm-pretty-tables overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-pretty-tables)
        (puthash ov t set)))
    set))

(defun gfm-pretty-tables--test-block-overlays (h-beg b-end)
  "Return gfm-pretty-tables overlays whose start lies in H-BEG..B-END."
  (cl-remove-if-not
   (lambda (ov)
     (and (overlay-get ov 'gfm-pretty-tables)
          (let ((s (overlay-start ov)))
            (and s (<= h-beg s) (<= s b-end)))))
   (overlays-in h-beg (1+ b-end))))

(ert-deftest lang-markdown/gfm-pretty-tables-scoped-edit-inside-single-table ()
  "Editing inside one table only rebuilds that table's overlays."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "| C | D |\n| - | - |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-tables--find-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (b1-h (nth 0 b1)) (b1-e (nth 3 b1))
           (b2-h (nth 0 b2)) (b2-e (nth 3 b2))
           (b1-before (gfm-pretty-tables--test-block-overlays b1-h b1-e))
           (b2-before (gfm-pretty-tables--test-block-overlays b2-h b2-e)))
      (gfm-pretty--state-set 'tables 'dirty-region
            (cons (1+ b1-h) (1- b1-e)))
      (gfm-pretty--scheduled-rebuild)
      (let ((b1-after (gfm-pretty-tables--test-block-overlays b1-h b1-e))
            (b2-after (gfm-pretty-tables--test-block-overlays b2-h b2-e)))
        (should-not (cl-intersection b1-before b1-after))
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))
        (should (cl-every (lambda (o) (memq o b2-before)) b2-after))))))

(ert-deftest lang-markdown/gfm-pretty-tables-scoped-edit-outside-tables-noop ()
  "Editing in a region intersecting no decorated table is a no-op."
  (with-temp-buffer
    (insert "intro line\n\n| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (let ((before (gfm-pretty-tables--test-overlay-set))
          (rebuild-count (plist-get (gfm-pretty--state-get 'tables 'rebuild-stats) :count)))
      (gfm-pretty--state-set 'tables 'dirty-region (cons 1 5))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-tables--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)
        (should (= rebuild-count
                   (plist-get (gfm-pretty--state-get 'tables 'rebuild-stats) :count)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-scoped-edit-spans-two-tables-full-rebuild ()
  "Edit region intersecting two tables triggers a full rebuild."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "| C | D |\n| - | - |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (let* ((blocks (gfm-pretty-tables--find-blocks))
           (b1-h (nth 0 (car blocks)))
           (b2-e (nth 3 (cadr blocks)))
           (before (gfm-pretty-tables--test-overlay-set)))
      (gfm-pretty--state-set 'tables 'dirty-region (cons b1-h b2-e))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-tables--test-overlay-set)))
        (should (cl-every (lambda (ov) (not (gethash ov after)))
                          (let (xs) (maphash (lambda (k _) (push k xs)) before)
                               xs)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-scoped-edit-overlapping-fence-full-rebuild ()
  "Edit region overlapping a code-fence line triggers a full rebuild."
  (with-temp-buffer
    (gfm-mode)
    (insert "```\nfenced\n```\n\n| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (let* ((fence (car (gfm-pretty-fences--find-blocks)))
           (open-line-beg (save-excursion
                            (goto-char (nth 0 fence)) (line-beginning-position)))
           (open-line-end (save-excursion
                            (goto-char (nth 0 fence)) (line-end-position)))
           (before (gfm-pretty-tables--test-overlay-set)))
      (gfm-pretty--state-set 'tables 'dirty-region (cons open-line-beg open-line-end))
      (gfm-pretty--scheduled-rebuild)
      (let ((after (gfm-pretty-tables--test-overlay-set)))
        ;; Full rebuild → overlay objects are fresh.
        (should (cl-every (lambda (ov) (not (gethash ov after)))
                          (let (xs) (maphash (lambda (k _) (push k xs)) before)
                               xs)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-per-window-display-overlays ()
  "Each window showing the buffer gets its own display-overlay set.
Anchor overlays stay shared across windows; only display overlays
carry a `window' restriction."
  (let ((buf (generate-new-buffer "*gfm-pretty-tables-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-pretty-tables))
                                (overlays-in (point-min) (point-max))))
                     (anchors (cl-count-if
                               (lambda (o) (overlay-get o 'gfm-pretty-tables-anchor))
                               overlays))
                     (displays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-pretty-tables-display))
                                overlays))
                     (n-displays (length displays))
                     (windowed (cl-count-if
                                (lambda (o) (overlay-get o 'window))
                                displays)))
                ;; Header + 1 body row → 2 anchors.
                (should (= 2 anchors))
                ;; 2 windows × 3 visible rows (header + delim + body) = 6.
                (should (= 6 n-displays))
                ;; All display overlays carry a `window' property.
                (should (= 6 windowed))))
            (delete-window other)))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-tables-reconcile-windows-touches-changed-only ()
  "Reconciling windows replaces only the resized window's display overlays.
Untouched windows keep their existing display-overlay objects (eq)."
  (let ((buf (generate-new-buffer "*gfm-pretty-tables-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (let* ((displays-for (lambda (w)
                                     (cl-remove-if-not
                                      (lambda (o)
                                        (and (overlay-get o 'gfm-pretty-tables-display)
                                             (eq (overlay-get o 'window) w)))
                                      (gfm-pretty--state-get 'tables 'overlays))))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                ;; Forge a width change for win-a only by mutating the cached
                ;; state; reconcile sees win-a as resized, win-b as unchanged.
                (gfm-pretty--state-set 'tables 'last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              (gfm-pretty--state-get 'tables 'last-window-state)))
                (gfm-pretty-tables--reconcile-windows)
                (let ((a-after (funcall displays-for win-a))
                      (b-after (funcall displays-for win-b)))
                  ;; win-a's overlays were replaced with fresh ones.
                  (should-not (cl-intersection a-before a-after))
                  ;; win-b's overlays are untouched (same objects, same count).
                  (should (= (length b-before) (length b-after)))
                  (should (cl-every (lambda (o) (memq o b-after)) b-before)))))
            (delete-window win-b)))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-tables-highlight-targets-selected-window ()
  "Active-cell highlight paints the selected window's display overlay only.
Per-window display overlays let two windows render the same buffer at
different widths; the cell-edit highlight should only affect the
window holding point."
  (let ((buf (generate-new-buffer "*gfm-pretty-tables-hl-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-pretty-mode 1)
              (goto-char (point-min))
              (forward-line 2)
              (forward-char 2)
              (with-selected-window win-a
                (gfm-pretty-tables--update-cursor-highlight))
              (let* ((info (gfm-pretty-tables--cell-info-at-point))
                     (anchor (car info))
                     (a-display (gfm-pretty-tables--display-overlay-for-anchor anchor win-a))
                     (b-display (gfm-pretty-tables--display-overlay-for-anchor anchor win-b)))
                ;; Selected window's overlay carries the saved-display sentinel.
                (should (overlay-get a-display 'gfm-pretty-saved-display))
                ;; The other window's overlay does not.
                (should-not (overlay-get b-display 'gfm-pretty-saved-display))))
            (delete-window win-b)))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-tables-window-config-change-wired-to-full-rebuild ()
  "`window-configuration-change-hook' carries the engine's single handler."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (should (memq 'gfm-pretty--wcc
                  window-configuration-change-hook))))

(ert-deftest lang-markdown/gfm-pretty-tables-block-visible-p ()
  "`gfm-pretty-tables--block-visible-p' detects overlap with any window range."
  (let ((block '(100 110 120 200)))
    ;; Block fully inside a single range.
    (should (gfm-pretty-tables--block-visible-p block '((50 . 250))))
    ;; Range fully inside block.
    (should (gfm-pretty-tables--block-visible-p block '((130 . 180))))
    ;; Edge-touching counts as visible.
    (should (gfm-pretty-tables--block-visible-p block '((50 . 100))))
    (should (gfm-pretty-tables--block-visible-p block '((200 . 250))))
    ;; No range overlaps.
    (should-not (gfm-pretty-tables--block-visible-p block '((1 . 99) (201 . 300))))
    ;; Empty list of ranges.
    (should-not (gfm-pretty-tables--block-visible-p block nil))
    ;; Multiple ranges; one covers it.
    (should (gfm-pretty-tables--block-visible-p block '((1 . 50) (130 . 180))))))

;;; Phase-level instrumentation

(ert-deftest lang-markdown/gfm-pretty-tables-phase-totals-non-negative-and-bounded ()
  "Phase totals are non-negative and sum to ≤ recorded total-time."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-pretty-mode 1)
    (let* ((stats (gfm-pretty--state-get 'tables 'rebuild-stats))
           (total (plist-get stats :total))
           (phases (gfm-pretty--state-get 'tables 'phase-totals)))
      (should phases)
      (dolist (p phases)
        (should (>= (cdr p) 0)))
      (let ((sum (cl-loop for p in phases sum (cdr p))))
        ;; Allow a small fudge for clock noise between outer total and phases.
        (should (<= sum (+ total 0.001)))))))

(ert-deftest lang-markdown/gfm-pretty-tables-phase-totals-include-required-keys ()
  "Phase totals include all five keys required by the spec."
  (with-temp-buffer
    (insert "| A |\n| - |\n| 1 |\n")
    (gfm-pretty-mode 1)
    (let ((phases (gfm-pretty--state-get 'tables 'phase-totals)))
      (dolist (k '(find-blocks parse layout compose apply))
        (should (assq k phases))))))

;;; Reveal

;;; gfm-pretty-links tests

(require 'gfm-pretty-links)

(defmacro lang-markdown-tests--with-links-buffer (contents &rest body)
  "Run BODY in a `markdown-mode' temp buffer holding CONTENTS, links mode on."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (markdown-mode))
     (setq-local markdown-hide-urls t)
     (insert ,contents)
     (gfm-pretty-mode 1)
     ,@body))

(defun lang-markdown-tests--link-overlays ()
  "Return gfm-pretty-links overlays in the current buffer, sorted by start."
  (sort (copy-sequence (gfm-pretty--state-get 'links 'overlays))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun lang-markdown-tests--link-overlay-at (pos &optional side)
  "Return a gfm-pretty-links overlay covering POS, optionally matching SIDE."
  (cl-find-if (lambda (o)
                (and (overlay-get o 'gfm-pretty-links-class)
                     (<= (overlay-start o) pos)
                     (< pos (overlay-end o))
                     (or (null side) (eq side (overlay-get o 'gfm-pretty-links-side)))))
              (gfm-pretty--state-get 'links 'overlays)))

;;; Mode toggle

(ert-deftest lang-markdown/gfm-pretty-links-mode-creates-overlays ()
  "Enabling the mode decorates a recognised link."
  (lang-markdown-tests--with-links-buffer
      "See [Anthropic](https://anthropic.com) here.\n"
    (should (lang-markdown-tests--link-overlay-at 6 'title))
    (should (lang-markdown-tests--link-overlay-at 18 'url))))

(ert-deftest lang-markdown/gfm-pretty-links-mode-removes-overlays ()
  "Disabling the mode removes every overlay it created."
  (lang-markdown-tests--with-links-buffer
      "See [Anthropic](https://anthropic.com) here.\n"
    (should (gfm-pretty--state-get 'links 'overlays))
    (gfm-pretty-mode -1)
    (should-not (gfm-pretty--state-get 'links 'overlays))
    (should-not (cl-some (lambda (o) (overlay-get o 'gfm-pretty-links))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-pretty-links-hide-urls-off-disables-mode ()
  "Setting `markdown-hide-urls' to nil disables the mode and clears overlays."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "See [Anthropic](https://anthropic.com).\n")
    (gfm-pretty-links--maybe-enable)
    (should (gfm-pretty--state-get 'links 'enabled-p))
    (setq-local markdown-hide-urls nil)
    (should-not (gfm-pretty--state-get 'links 'enabled-p))
    (should-not (gfm-pretty--state-get 'links 'overlays))))

(ert-deftest lang-markdown/gfm-pretty-links-enabled-via-gfm-mode-hook ()
  "Links activation runs via the `gfm-pretty-mode' hook."
  (should (memq 'gfm-pretty-mode gfm-mode-hook)))

;;; 14.1 Per-shape decoration

(ert-deftest lang-markdown/gfm-pretty-links-inline-with-title-attr ()
  "Inline link with a title attribute is decorated and exposes the attr."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com \"official\")\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (eq 'inline (overlay-get ov 'gfm-pretty-links-kind)))
      (should (equal "https://anthropic.com" (overlay-get ov 'gfm-pretty-links-url)))
      (should (equal "official" (overlay-get ov 'gfm-pretty-links-title-attr)))
      (should (equal "Anthropic"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-pretty-links-reference-full ()
  "Full reference link resolves through the definition alist."
  (lang-markdown-tests--with-links-buffer
      "[docs][d] here.\n\n[d]: https://example.com\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (eq 'reference (overlay-get ov 'gfm-pretty-links-kind)))
      (should (equal "https://example.com" (overlay-get ov 'gfm-pretty-links-url))))))

(ert-deftest lang-markdown/gfm-pretty-links-reference-collapsed ()
  "Collapsed reference link `[text][]' resolves using the text as label."
  (lang-markdown-tests--with-links-buffer
      "[design][] here.\n\n[design]: ./docs/adr-001.md\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (equal "./docs/adr-001.md" (overlay-get ov 'gfm-pretty-links-url))))))

(ert-deftest lang-markdown/gfm-pretty-links-reference-shortcut ()
  "Shortcut reference `[label]' is decorated only when a definition exists."
  (lang-markdown-tests--with-links-buffer
      "Prose [design] more.\n\n[design]: ./adr.md\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'reference (overlay-get ov 'gfm-pretty-links-kind)))
      (should (equal "./adr.md" (overlay-get ov 'gfm-pretty-links-url))))))

(ert-deftest lang-markdown/gfm-pretty-links-reference-shortcut-undefined-not-decorated ()
  "Shortcut `[label]' with no matching definition is left raw."
  (lang-markdown-tests--with-links-buffer
      "Prose [design] more, no definition.\n"
    (should-not (lang-markdown-tests--link-overlay-at 8))))

(ert-deftest lang-markdown/gfm-pretty-links-autolink-host-label ()
  "An autolink is decorated with the host as the visible label."
  (lang-markdown-tests--with-links-buffer
      "Visit <https://anthropic.com/path> now.\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'autolink (overlay-get ov 'gfm-pretty-links-kind)))
      (should (equal "anthropic.com"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-pretty-links-bare-url ()
  "A GFM bare URL is decorated."
  (lang-markdown-tests--with-links-buffer
      "Visit https://anthropic.com today.\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'bare-url (overlay-get ov 'gfm-pretty-links-kind)))
      (should (equal "anthropic.com"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-pretty-links-wiki-link ()
  "A wiki link is decorated when `markdown-enable-wiki-links' is on."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (setq-local markdown-enable-wiki-links t)
    (insert "See [[Some Page]] here.\n")
    (gfm-pretty-mode 1)
    (let ((ov (lang-markdown-tests--link-overlay-at 6 'title)))
      (should ov)
      (should (eq 'wiki (overlay-get ov 'gfm-pretty-links-kind))))))

(ert-deftest lang-markdown/gfm-pretty-links-image-not-decorated ()
  "Image links are explicitly left raw."
  (lang-markdown-tests--with-links-buffer
      "![alt](./diagram.png)\n"
    (should-not (gfm-pretty--state-get 'links 'overlays))))

(ert-deftest lang-markdown/gfm-pretty-links-reference-definition-not-decorated ()
  "Reference-definition lines themselves are not decorated."
  (lang-markdown-tests--with-links-buffer
      "[d]: https://example.com\n"
    (should-not (gfm-pretty--state-get 'links 'overlays))))

;;; 14.1b Code-region exclusion

(ert-deftest lang-markdown/gfm-pretty-links-in-code-p-fenced ()
  "`gfm-pretty-links--in-code-p' is non-nil inside a fenced code block."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (insert "Prose line.\n\n```\n[foo](bar)\n```\n\nMore prose.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[foo")
    (should (gfm-pretty-links--in-code-p (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-in-code-p-indented ()
  "`gfm-pretty-links--in-code-p' is non-nil inside an indented code block."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (insert "Prose line.\n\n    [foo](bar)\n\nMore prose.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[foo")
    (should (gfm-pretty-links--in-code-p (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-in-code-p-inline ()
  "`gfm-pretty-links--in-code-p' is non-nil inside an inline code span."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (insert "Prose `[foo](bar)` end.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[foo")
    (should (gfm-pretty-links--in-code-p (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-in-code-p-prose ()
  "`gfm-pretty-links--in-code-p' is nil for ordinary prose."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (insert "See [foo](bar) here.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[foo")
    (should-not (gfm-pretty-links--in-code-p (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-in-code-p-adjacent ()
  "`gfm-pretty-links--in-code-p' is nil immediately outside a code span."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (insert "See [foo](bar) and `code` end.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "[foo")
    (should-not (gfm-pretty-links--in-code-p (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-skip-fenced-block ()
  "`[foo](bar)' inside a fenced code block is not decorated."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "Prose line.\n\n```\n[foo](bar)\n```\n\nMore prose.\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "[foo")
    (should-not (lang-markdown-tests--link-overlay-at (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-skip-indented-block ()
  "`[foo](bar)' inside an indented code block is not decorated."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "Prose line.\n\n    [foo](bar)\n\nMore prose.\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "[foo")
    (should-not (lang-markdown-tests--link-overlay-at (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-skip-inline-code ()
  "`[foo](bar)' inside an inline code span is not decorated."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "Prose `[foo](bar)` end.\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "[foo")
    (should-not (lang-markdown-tests--link-overlay-at (match-beginning 0)))))

(ert-deftest lang-markdown/gfm-pretty-links-adjacent-code-still-decorates ()
  "A link adjacent to but outside an inline code span decorates normally."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "See [foo](https://x) and `code` end.\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (search-forward "[foo")
    (should (lang-markdown-tests--link-overlay-at (match-beginning 0) 'title))))

;;; 14.2 Reference resolution

(ert-deftest lang-markdown/gfm-pretty-links-ref-def-alist-build ()
  "The ref-def alist maps a downcased label to its URL, title, and position."
  (lang-markdown-tests--with-links-buffer
      "[D]: https://example.com \"a title\"\n"
    (let ((entry (gfm-pretty-links--resolve-ref "d")))
      (should entry)
      (should (equal "https://example.com" (nth 0 entry)))
      (should (equal "a title" (nth 1 entry)))
      (should (integerp (nth 2 entry))))))

(ert-deftest lang-markdown/gfm-pretty-links-ref-def-first-wins ()
  "When a label is defined twice, the first definition wins."
  (lang-markdown-tests--with-links-buffer
      "[d]: https://first.example\n[d]: https://second.example\n"
    (should (equal "https://first.example" (nth 0 (gfm-pretty-links--resolve-ref "d"))))))

(ert-deftest lang-markdown/gfm-pretty-links-broken-reference-not-decorated ()
  "A reference link whose label has no definition is not decorated."
  (lang-markdown-tests--with-links-buffer
      "[title][missing] here.\n"
    (should-not (lang-markdown-tests--link-overlay-at 2))))

(ert-deftest lang-markdown/gfm-pretty-links-ref-def-recomputed-on-rebuild ()
  "Editing a definition line and rebuilding re-resolves reference links."
  (lang-markdown-tests--with-links-buffer
      "[docs][d]\n\n[d]: https://old.example\n"
    (should (equal "https://old.example"
                   (overlay-get (lang-markdown-tests--link-overlay-at 2 'title)
                                'gfm-pretty-links-url)))
    (goto-char (point-min))
    (search-forward "https://old.example")
    (replace-match "https://new.example")
    (gfm-pretty-links--rebuild)
    (should (equal "https://new.example"
                   (overlay-get (lang-markdown-tests--link-overlay-at 2 'title)
                                'gfm-pretty-links-url)))))

;;; 5.x Icon resolution

(ert-deftest lang-markdown/gfm-pretty-links-icon-for-http-url ()
  "An http(s) URL resolves through `nerd-icons-icon-for-url'."
  (skip-unless (fboundp 'nerd-icons-icon-for-url))
  (should (equal (nerd-icons-icon-for-url "https://github.com/foo/bar")
                 (gfm-pretty-links--icon-for-target "https://github.com/foo/bar"))))

(ert-deftest lang-markdown/gfm-pretty-links-icon-for-relative-file ()
  "A relative path resolves through `nerd-icons-icon-for-file' on the basename."
  (skip-unless (fboundp 'nerd-icons-icon-for-file))
  (should (equal (nerd-icons-icon-for-file "init.el")
                 (gfm-pretty-links--icon-for-target "./modules/init.el"))))

(ert-deftest lang-markdown/gfm-pretty-links-label-for-naked-url ()
  "The naked-URL label is the URL host."
  (should (equal "anthropic.com"
                 (gfm-pretty-links--label-for-naked-url
                  "https://anthropic.com/some/path"))))

;;; 7.x Suppression of the built-in compose path

(ert-deftest lang-markdown/gfm-pretty-links-suppresses-composition ()
  "With the mode on, the URL region gets no `composition' text property."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "(")
    (should-not (get-text-property (point) 'composition))))

(ert-deftest lang-markdown/gfm-pretty-links-composition-applies-when-mode-off ()
  "With the mode off but `markdown-hide-urls' on, composition still applies."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "[Anthropic](https://anthropic.com)\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "(")
    (should (get-text-property (point) 'composition))))

;;; 8.x URL classifier

(ert-deftest lang-markdown/gfm-pretty-links-classify-url-anchor ()
  "Same-document anchors classify as `anchor'."
  (should (eq 'anchor (gfm-pretty-links--classify-url "#setup")))
  (should (eq 'anchor (gfm-pretty-links--classify-url "#")))
  (should (eq 'anchor (gfm-pretty-links--classify-url "#with-dash-and-1"))))

(ert-deftest lang-markdown/gfm-pretty-links-classify-url-file ()
  "Relative paths, absolute paths, and `file:' URIs classify as `file'."
  (should (eq 'file (gfm-pretty-links--classify-url "./scripts/x.sh")))
  (should (eq 'file (gfm-pretty-links--classify-url "../sibling/x.md")))
  (should (eq 'file (gfm-pretty-links--classify-url "/etc/hostname")))
  (should (eq 'file (gfm-pretty-links--classify-url "file:/tmp/x"))))

(ert-deftest lang-markdown/gfm-pretty-links-classify-url-web ()
  "Schemed URLs, hostless URLs, and unknown shapes default to `web'."
  (should (eq 'web (gfm-pretty-links--classify-url "https://anthropic.com")))
  (should (eq 'web (gfm-pretty-links--classify-url "http://x.example/p")))
  (should (eq 'web (gfm-pretty-links--classify-url "mailto:a@b.com")))
  (should (eq 'web (gfm-pretty-links--classify-url "ftp://x.example")))
  (should (eq 'web (gfm-pretty-links--classify-url "anthropic.com"))))

(ert-deftest lang-markdown/gfm-pretty-links-classify-url-nil-or-empty ()
  "nil / empty URL defaults to `web' (no special handling needed)."
  (should (eq 'web (gfm-pretty-links--classify-url nil)))
  (should (eq 'web (gfm-pretty-links--classify-url ""))))

(ert-deftest lang-markdown/gfm-pretty-links-classify-reference-via-resolved-url ()
  "Reference links classify by their resolved definition URL, not source shape."
  (lang-markdown-tests--with-links-buffer
      "[ops][tg]\n\n[tg]: ./scripts/tg.sh\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should (eq 'file (overlay-get ov 'gfm-pretty-links-class))))))

(ert-deftest lang-markdown/gfm-pretty-links-classify-reference-web ()
  "A reference link whose definition is a web URL classifies as `web'."
  (lang-markdown-tests--with-links-buffer
      "[catalog][cat]\n\n[cat]: https://github.com/x/y\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should (eq 'web (overlay-get ov 'gfm-pretty-links-class))))))

;;; 8.y Local-link faces

(ert-deftest lang-markdown/gfm-pretty-links-anchor-face-strips-underline ()
  "Default `gfm-pretty-links-anchor-face' resolves with `:underline nil'."
  (should-not (face-attribute 'gfm-pretty-links-anchor-face :underline nil t)))

(ert-deftest lang-markdown/gfm-pretty-links-file-face-strips-underline ()
  "Default `gfm-pretty-links-file-face' resolves with `:underline nil'."
  (should-not (face-attribute 'gfm-pretty-links-file-face :underline nil t)))

(ert-deftest lang-markdown/gfm-pretty-links-anchor-face-inherits-link ()
  "`gfm-pretty-links-anchor-face' inherits `markdown-link-face'."
  (should (eq 'markdown-link-face
              (face-attribute 'gfm-pretty-links-anchor-face :inherit nil t))))

(ert-deftest lang-markdown/gfm-pretty-links-file-face-inherits-link ()
  "`gfm-pretty-links-file-face' inherits `markdown-link-face'."
  (should (eq 'markdown-link-face
              (face-attribute 'gfm-pretty-links-file-face :inherit nil t))))

;;; 8.z Title-side face by class

(defun lang-markdown-tests--display-face (ov)
  "Return the face applied to OV's `display' string."
  (get-text-property 0 'face (overlay-get ov 'display)))

(ert-deftest lang-markdown/gfm-pretty-links-title-face-web ()
  "A web link's title overlay uses `gfm-pretty-links-title-face'."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should (eq 'markdown-link-face (lang-markdown-tests--display-face ov))))))

(ert-deftest lang-markdown/gfm-pretty-links-title-face-anchor ()
  "An anchor link's title overlay uses `gfm-pretty-links-anchor-face'."
  (lang-markdown-tests--with-links-buffer
      "[Setup](#setup)\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should (eq 'gfm-pretty-links-anchor-face
                  (lang-markdown-tests--display-face ov))))))

(ert-deftest lang-markdown/gfm-pretty-links-title-face-file ()
  "A file link's title overlay uses `gfm-pretty-links-file-face'."
  (lang-markdown-tests--with-links-buffer
      "[ops](./scripts/x.sh)\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should (eq 'gfm-pretty-links-file-face
                  (lang-markdown-tests--display-face ov))))))

;;; 8.w URL-side suppression for local links

(defun lang-markdown-tests--link-overlay-count-for (kind)
  "Return the count of overlays for the link of KIND in the current buffer."
  (length (cl-remove-if-not
           (lambda (o) (overlay-get o 'gfm-pretty-links-class))
           (gfm-pretty--state-get 'links 'overlays))))

(ert-deftest lang-markdown/gfm-pretty-links-web-link-has-both-overlays ()
  "A web link produces both a title-side and a url-side overlay."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (should (lang-markdown-tests--link-overlay-at 2 'title))
    (should (lang-markdown-tests--link-overlay-at 13 'url))
    (should (= 2 (lang-markdown-tests--link-overlay-count-for 'inline)))))

(ert-deftest lang-markdown/gfm-pretty-links-anchor-link-hides-url-span ()
  "An anchor link produces a url-side overlay with empty `display'."
  (lang-markdown-tests--with-links-buffer
      "[Setup](#setup)\n"
    (should (lang-markdown-tests--link-overlay-at 2 'title))
    (let ((ov (lang-markdown-tests--link-overlay-at 9 'url)))
      (should ov)
      (should (equal "" (overlay-get ov 'display))))
    (should (= 2 (lang-markdown-tests--link-overlay-count-for 'inline)))))

(ert-deftest lang-markdown/gfm-pretty-links-anchor-url-overlay-keeps-metadata ()
  "The hidden anchor url overlay still carries RET-dispatch metadata."
  (lang-markdown-tests--with-links-buffer
      "[Setup](#setup)\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 9 'url)))
      (should ov)
      (should (eq 'anchor (overlay-get ov 'gfm-pretty-links-class)))
      (should (equal "#setup" (overlay-get ov 'gfm-pretty-links-url))))))

(ert-deftest lang-markdown/gfm-pretty-links-file-link-has-no-url-overlay ()
  "A file link produces only a title-side overlay (no icon)."
  (lang-markdown-tests--with-links-buffer
      "[ops](./scripts/x.sh)\n"
    (should (lang-markdown-tests--link-overlay-at 2 'title))
    (should (= 1 (lang-markdown-tests--link-overlay-count-for 'inline)))
    (should-not (cl-find-if
                 (lambda (o) (eq 'url (overlay-get o 'gfm-pretty-links-side)))
                 (gfm-pretty--state-get 'links 'overlays)))))

;;; 9.x RET / follow-link

(ert-deftest lang-markdown/gfm-pretty-links-ret-bound-on-title-overlay ()
  "`RET' resolves to the follow command inside a decorated link only."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain prose.\n"
    (goto-char 3)
    (should (eq 'gfm-pretty-links-follow-link-at-point (key-binding (kbd "RET"))))
    (goto-char (point-max))
    (should-not (eq 'gfm-pretty-links-follow-link-at-point (key-binding (kbd "RET"))))))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-browses-url ()
  "RET on a web link calls `markdown--browse-url' on the resolved URL."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (let (browsed)
      (cl-letf (((symbol-function 'markdown--browse-url)
                 (lambda (url) (setq browsed url))))
        (goto-char 3)
        (gfm-pretty-links-follow-link-at-point)
        (should (equal "https://anthropic.com" browsed))))))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-off-link-errors ()
  "`gfm-pretty-links-follow-link-at-point' off any decorated link signals a user error."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain.\n"
    (goto-char (point-max))
    (should-error (gfm-pretty-links-follow-link-at-point) :type 'user-error)))

(ert-deftest lang-markdown/gfm-pretty-links-heading-slug-basic ()
  "Heading slug is lowercase, space → hyphen, punctuation dropped."
  (should (equal "setup-steps" (gfm-pretty-links--heading-slug "Setup Steps")))
  (should (equal "what-now" (gfm-pretty-links--heading-slug "What, now?")))
  (should (equal "code_x" (gfm-pretty-links--heading-slug "Code_X"))))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-anchor-jumps-to-heading ()
  "RET on an anchor link moves point to the heading whose slug matches."
  (lang-markdown-tests--with-links-buffer
      "## Setup Steps\n\nPara [go](#setup-steps).\n"
    (let* ((heading-pos (save-excursion
                          (goto-char (point-min))
                          (search-forward "## Setup Steps")
                          (line-beginning-position))))
      (goto-char (point-min))
      (search-forward "[go]")
      (goto-char (1+ (match-beginning 0)))
      (gfm-pretty-links-follow-link-at-point)
      (should (= heading-pos (point))))))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-anchor-no-match-errors ()
  "RET on an anchor with no matching heading signals `user-error'."
  (lang-markdown-tests--with-links-buffer
      "Para [go](#missing).\n"
    (goto-char (point-min))
    (search-forward "[go]")
    (goto-char (1+ (match-beginning 0)))
    (should-error (gfm-pretty-links-follow-link-at-point) :type 'user-error)))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-file-expands-against-buffer-file ()
  "RET on a file link opens `find-file' with the path expanded against the buffer's directory."
  (let ((visited nil))
    (lang-markdown-tests--with-links-buffer
        "[ops](./scripts/x.sh)\n"
      (setq buffer-file-name "/repo/README.md")
      (cl-letf (((symbol-function 'find-file)
                 (lambda (p) (setq visited p))))
        (goto-char (point-min))
        (search-forward "[ops]")
        (goto-char (1+ (match-beginning 0)))
        (gfm-pretty-links-follow-link-at-point))
      (should (equal "/repo/scripts/x.sh" visited)))))

(ert-deftest lang-markdown/gfm-pretty-links-follow-link-file-fileless-buffer ()
  "In a buffer with no `buffer-file-name', file links resolve against `default-directory'."
  (lang-markdown-tests--with-links-buffer
      "[ops](./scripts/x.sh)\n"
    (let ((default-directory "/tmp/gfm-pretty-test/")
          visited)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (p) (setq visited p))))
        (goto-char (point-min))
        (search-forward "[ops]")
        (goto-char (1+ (match-beginning 0)))
        (gfm-pretty-links-follow-link-at-point))
      (should (equal "/tmp/gfm-pretty-test/scripts/x.sh" visited)))))

;;; 10.x xref backend

(ert-deftest lang-markdown/gfm-pretty-links-xref-backend-claims-reference-link ()
  "The xref backend claims a reference link and defers off one."
  (lang-markdown-tests--with-links-buffer
      "[docs][d] then [inline](https://x.example)\n\n[d]: https://d.example\n"
    (goto-char 3)
    (should (eq 'gfm-pretty-links (gfm-pretty-links--xref-backend)))
    (goto-char (+ (point-min) 17))      ; inside the inline link
    (should-not (gfm-pretty-links--xref-backend))))

(ert-deftest lang-markdown/gfm-pretty-links-xref-jumps-to-definition ()
  "`xref-backend-definitions' points at the `[label]:' definition line."
  (lang-markdown-tests--with-links-buffer
      "[docs][adr] here.\n\n[adr]: https://adr.example\n"
    (goto-char 3)
    (let* ((id (xref-backend-identifier-at-point 'gfm-pretty-links))
           (defs (xref-backend-definitions 'gfm-pretty-links id))
           (def-pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "[adr]:")
                      (line-beginning-position))))
      (should (equal "adr" id))
      (should (= 1 (length defs)))
      (should (= def-pos
                 (xref-location-marker (xref-item-location (car defs))))))))

;;; 11.x Eldoc

(defun lang-markdown-tests--face-at (str pos)
  "Return the `face' text property of STR at POS."
  (get-text-property pos 'face str))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-inline-web-format ()
  "Eldoc on an inline web link renders `[title](url)' with class-aware faces."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (goto-char 3)
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (stringp doc))
      (should (equal "[Anthropic](https://anthropic.com)"
                     (substring-no-properties doc)))
      ;; `[' shadowed.
      (should (eq 'shadow (lang-markdown-tests--face-at doc 0)))
      ;; Title in title face.
      (should (eq 'markdown-link-face (lang-markdown-tests--face-at doc 1)))
      ;; `](' shadowed (position 10 is `]').
      (should (eq 'shadow (lang-markdown-tests--face-at doc 10)))
      ;; URL in url face (position 12 is `h' of https).
      (should (eq 'markdown-url-face (lang-markdown-tests--face-at doc 12)))
      ;; closing `)'.
      (should (eq 'shadow
                  (lang-markdown-tests--face-at doc (1- (length doc))))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-inline-anchor-format ()
  "Eldoc on an inline anchor link uses the anchor face on the title."
  (lang-markdown-tests--with-links-buffer
      "[Setup](#setup)\n"
    (goto-char 3)
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (equal "[Setup](#setup)" (substring-no-properties doc)))
      (should (eq 'gfm-pretty-links-anchor-face
                  (lang-markdown-tests--face-at doc 1))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-inline-file-format ()
  "Eldoc on an inline file link uses the file face on the title."
  (lang-markdown-tests--with-links-buffer
      "[ops](./scripts/x.sh)\n"
    (goto-char 3)
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (equal "[ops](./scripts/x.sh)" (substring-no-properties doc)))
      (should (eq 'gfm-pretty-links-file-face
                  (lang-markdown-tests--face-at doc 1))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-reference-format ()
  "Eldoc on a full reference link renders `[title][label]' with shadow on brackets."
  (lang-markdown-tests--with-links-buffer
      "[ops][tg]\n\n[tg]: ./scripts/x.sh\n"
    (goto-char 3)
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (equal "[ops][tg]" (substring-no-properties doc)))
      ;; Title and label both in file face.
      (should (eq 'gfm-pretty-links-file-face
                  (lang-markdown-tests--face-at doc 1)))
      (should (eq 'gfm-pretty-links-file-face
                  (lang-markdown-tests--face-at doc 6)))
      ;; Brackets shadowed.
      (should (eq 'shadow (lang-markdown-tests--face-at doc 0)))
      (should (eq 'shadow (lang-markdown-tests--face-at doc 4))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-shortcut-format ()
  "Eldoc on a shortcut reference renders `[label]' (no second bracket pair)."
  (lang-markdown-tests--with-links-buffer
      "Prose [design] more.\n\n[design]: ./adr.md\n"
    (goto-char (point-min))
    (search-forward "[design]")
    (goto-char (1+ (match-beginning 0)))
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (equal "[design]" (substring-no-properties doc))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-inline-title-attr-suffix ()
  "Eldoc on an inline link with a title attribute appends ` — \"…\"' italic."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com \"official\")\n"
    (goto-char 3)
    (let ((doc (gfm-pretty-links--eldoc-function #'ignore)))
      (should (string-match-p "\"official\"" (substring-no-properties doc)))
      (should (string-match-p " — " (substring-no-properties doc)))
      (let* ((s (substring-no-properties doc))
             (pos (string-match "\"" s)))
        (should (eq 'italic (lang-markdown-tests--face-at doc pos)))))))

(ert-deftest lang-markdown/gfm-pretty-links-eldoc-returns-nil-off-link ()
  "Eldoc returns nil when point is not on a decorated link."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain prose.\n"
    (goto-char (point-max))
    (should-not (gfm-pretty-links--eldoc-function #'ignore))))

;;; 14.8 Width walker — table cell with a decorated link

(ert-deftest lang-markdown/gfm-pretty-links-table-cell-width-counts-decoration ()
  "A table cell with a decorated link sizes its column by the visible width."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "| [Anthropic](https://anthropic.com) |\n| --- |\n| x |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (let* ((cells (gfm-pretty-tables--fontify-row-cells
                   (line-beginning-position) (line-end-position)))
           (w (gfm-pretty-tables--visible-width (car cells))))
      ;; "Anthropic" (9) + URL icon (1) = 10, well under the raw 34 chars.
      (should (<= w 12))
      (should (< w (length "[Anthropic](https://anthropic.com)"))))))

(ert-deftest lang-markdown/gfm-pretty-links-table-cell-bakes-in-decoration ()
  "A table cell's fontified string has the link decoration spliced in.
The raw bracket/URL text is gone — `display' text properties nested in
an overlay's `display' string are not honoured by redisplay, so the
decoration must be baked into the cell string itself."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "| [Anthropic](https://anthropic.com) |\n| --- |\n| x |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (let* ((cell (car (gfm-pretty-tables--fontify-row-cells
                       (line-beginning-position) (line-end-position))))
           (plain (substring-no-properties cell)))
      (should (string-match-p "Anthropic" plain))
      (should-not (string-match-p "(https://anthropic\\.com)" plain))
      (should-not (string-match-p "\\[Anthropic\\]" plain)))))

;;; 14.9 Narrowing-regression

(defun lang-markdown-tests--two-slide-links-buffer ()
  "Insert a two-slide buffer with one decorated link per slide."
  (insert "Slide one: [Anthropic](https://anthropic.com).\n")
  (insert "\n# slide 2\n\n")
  (insert "Slide two: [Example](https://example.com).\n"))

(ert-deftest lang-markdown/gfm-pretty-links-narrow-rebuild-widen-rebuild-converges ()
  "narrow -> rebuild -> widen -> rebuild matches a clean widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (lang-markdown-tests--two-slide-links-buffer)
    (gfm-pretty-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-pretty-links))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty-links--rebuild)
      (widen)
      (gfm-pretty-links--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-pretty-links))))))

;;; gfm-pretty-hrule tests

(require 'gfm-pretty-hrule)

(defun lang-markdown-tests--hrule-bar-overlays ()
  "Return display overlays carrying the HR bar."
  (cl-remove-if-not
   (lambda (ov) (eq (overlay-get ov 'gfm-pretty-hrule-kind) 'bar))
   (overlays-in (point-min) (point-max))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-detects-dash-form ()
  "Dash-form HR lines are discovered via the `markdown-hr' property."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n---\n\nbar\n")
    (font-lock-ensure)
    (let ((blocks (gfm-pretty-hrule--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-ignores-asterisk-form ()
  "Asterisk-form HR lines are not discovered."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n***\n\nbar\n")
    (font-lock-ensure)
    (should-not (gfm-pretty-hrule--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-ignores-underscore-form ()
  "Underscore-form HR lines are not discovered."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n___\n\nbar\n")
    (font-lock-ensure)
    (should-not (gfm-pretty-hrule--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-ignores-setext-2-underline ()
  "A setext-2 heading underline is not discovered as an HR."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "Heading\n---\n\nbody\n")
    (font-lock-ensure)
    (should-not (gfm-pretty-hrule--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-ignores-dashes-in-fenced-code ()
  "Dash-only lines inside a fenced code block are not discovered."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "```\n---\n```\n")
    (font-lock-ensure)
    (should-not (gfm-pretty-hrule--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-hrule-find-blocks-ignores-blockquote-form ()
  "A `> ---' line inside a blockquote is not discovered."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n> ---\n\nbar\n")
    (font-lock-ensure)
    (should-not (gfm-pretty-hrule--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-creates-bar-overlay ()
  "Enabling the mode creates a bar display overlay over the HR line."
  (let ((buf (generate-new-buffer "*gfm-pretty-hrule-mode-test*")))
    (unwind-protect
        (with-current-buffer buf
          (delay-mode-hooks (gfm-mode))
          (insert "\nfoo\n\n---\n\nbar\n")
          (font-lock-ensure)
          (gfm-pretty-mode 1)
          (let ((ovs (lang-markdown-tests--hrule-bar-overlays)))
            (should (= 1 (length ovs)))
            (let* ((ov (car ovs))
                   (display (overlay-get ov 'display)))
              (should (stringp display))
              (should (string-match-p "\\`─+\\'" display))
              (should (overlay-get ov 'gfm-pretty-hrule-revealable))
              (should (overlay-get ov 'evaporate)))))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-asterisk-produces-no-overlay ()
  "Asterisk-form HR does not produce a bar overlay."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n***\n\nbar\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (should-not (lang-markdown-tests--hrule-bar-overlays))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-setext-produces-no-overlay ()
  "A setext-2 underline produces no bar overlay."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "Heading\n---\n\nbody\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (should-not (lang-markdown-tests--hrule-bar-overlays))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-fenced-code-produces-no-overlay ()
  "An HR-looking line inside a fenced code block produces no overlay."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "```\n---\n```\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (should-not (lang-markdown-tests--hrule-bar-overlays))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-blockquote-produces-no-overlay ()
  "A `> ---' line inside a blockquote produces no bar overlay."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n> ---\n\nbar\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (should-not (lang-markdown-tests--hrule-bar-overlays))))

(ert-deftest lang-markdown/gfm-pretty-hrule-mode-removes-overlays ()
  "Disabling the mode removes all bar overlays."
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (insert "\nfoo\n\n---\n\nbar\n")
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (gfm-pretty-mode -1)
    (should-not (lang-markdown-tests--hrule-bar-overlays))))

(ert-deftest lang-markdown/gfm-pretty-hrule-reveal-suppresses-display-at-point ()
  "Point on the HR line suppresses the bar; moving off restores it."
  (let ((buf (generate-new-buffer "*gfm-pretty-hrule-reveal-test*")))
    (unwind-protect
        (with-current-buffer buf
          (delay-mode-hooks (gfm-mode))
          (insert "\nfoo\n\n---\n\nbar\n")
          (font-lock-ensure)
          (gfm-pretty-mode 1)
          (let* ((ov (car (lang-markdown-tests--hrule-bar-overlays)))
                 (hr-pos (and ov (overlay-start ov))))
            (should ov)
            (should (stringp (overlay-get ov 'display)))
            (goto-char hr-pos)
            (gfm-pretty--reveal)
            (should (null (overlay-get ov 'display)))
            (goto-char (point-min))
            (gfm-pretty--reveal)
            (should (stringp (overlay-get ov 'display)))))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-pretty-hrule-enabled-via-gfm-mode-hook ()
  "`gfm-pretty-hrule-mode' is wired into `gfm-mode-hook'."
  (should (memq 'gfm-pretty-mode gfm-mode-hook)))

;;; Narrowing-regression — gfm-pretty-hrule

(defun lang-markdown-tests--two-slide-hrule-buffer ()
  "Insert a two-slide buffer with one HR per slide."
  (insert "first\n\n---\n\nbody one\n")
  (insert "\n# slide 2\n\n")
  (insert "second\n\n---\n\nbody two\n"))

(ert-deftest lang-markdown/gfm-pretty-hrule-narrow-rebuild-widen-rebuild-converges ()
  "narrow -> rebuild -> widen -> rebuild matches a clean widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (delay-mode-hooks (gfm-mode))
    (lang-markdown-tests--two-slide-hrule-buffer)
    (font-lock-ensure)
    (gfm-pretty-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-pretty-hrule))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-pretty-hrule--rebuild)
      (widen)
      (gfm-pretty-hrule--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-pretty-hrule))))))

;;; gfm-pretty umbrella mode

(ert-deftest gfm-pretty/umbrella-enables-all-decorators ()
  "Enabling `gfm-pretty-mode' turns on each registered decorator."
  (require 'gfm-pretty-callouts)
  (require 'gfm-pretty-fences)
  (require 'gfm-pretty-tables)
  (require 'gfm-pretty-hrule)
  (with-temp-buffer
    (gfm-pretty-mode 1)
    (should (gfm-pretty--state-get 'callouts 'enabled-p))
    (should (gfm-pretty--state-get 'fences 'enabled-p))
    (should (gfm-pretty--state-get 'tables 'enabled-p))
    (should (gfm-pretty--state-get 'hrule 'enabled-p))
    (gfm-pretty-mode -1)
    (should-not (gfm-pretty--state-get 'callouts 'enabled-p))
    (should-not (gfm-pretty--state-get 'fences 'enabled-p))
    (should-not (gfm-pretty--state-get 'tables 'enabled-p))
    (should-not (gfm-pretty--state-get 'hrule 'enabled-p))))

(ert-deftest gfm-pretty/umbrella-installs-one-engine-hook-per-event ()
  "Engine installs exactly one of its own handler per lifecycle hook.
Decorators may install their own extras via `:on-enable-fn' (tables
cursor handler, links xref / eldoc) — those are not the engine's
hooks and are checked separately."
  (require 'gfm-pretty-callouts)
  (require 'gfm-pretty-fences)
  (require 'gfm-pretty-tables)
  (require 'gfm-pretty-hrule)
  (require 'gfm-pretty-links)
  (with-temp-buffer
    (let ((ac-before (cl-count #'gfm-pretty--after-change
                               after-change-functions))
          (wcc-before (cl-count #'gfm-pretty--wcc
                                window-configuration-change-hook))
          (pc-before (cl-count #'gfm-pretty--reveal
                               post-command-hook)))
      (gfm-pretty-mode 1)
      (should (= 1 (- (cl-count #'gfm-pretty--after-change
                                after-change-functions)
                      ac-before)))
      (should (= 1 (- (cl-count #'gfm-pretty--wcc
                                window-configuration-change-hook)
                      wcc-before)))
      (should (= 1 (- (cl-count #'gfm-pretty--reveal
                                post-command-hook)
                      pc-before)))
      (gfm-pretty-mode -1)
      (should (= ac-before (cl-count #'gfm-pretty--after-change
                                     after-change-functions)))
      (should (= wcc-before (cl-count #'gfm-pretty--wcc
                                      window-configuration-change-hook)))
      (should (= pc-before (cl-count #'gfm-pretty--reveal
                                     post-command-hook))))))

(ert-deftest gfm-pretty/engine-arms-at-most-one-rebuild-timer ()
  "Editing the buffer arms at most one engine-owned idle rebuild timer."
  (with-temp-buffer
    (gfm-pretty-mode 1)
    (insert "> [!NOTE]\n> Hello.\n")
    (should (or (null gfm-pretty--rebuild-timer)
                (timerp gfm-pretty--rebuild-timer)))
    (when (timerp gfm-pretty--rebuild-timer)
      (cancel-timer gfm-pretty--rebuild-timer)
      (setq gfm-pretty--rebuild-timer nil))
    (gfm-pretty-mode -1)))

(ert-deftest gfm-pretty/toggle-decorator-flips ()
  "`gfm-pretty-toggle-decorator' toggles a single decorator."
  (require 'gfm-pretty-fences)
  (with-temp-buffer
    (should-not (gfm-pretty--state-get 'fences 'enabled-p))
    (gfm-pretty-toggle-decorator 'fences)
    (should (gfm-pretty--state-get 'fences 'enabled-p))
    (gfm-pretty-toggle-decorator 'fences)
    (should-not (gfm-pretty--state-get 'fences 'enabled-p))))

(ert-deftest gfm-pretty/block-at-point-dispatches-to-tables ()
  "`gfm-pretty-block-at-point' returns (tables . BLOCK) inside a table."
  (require 'gfm-pretty-tables)
  (require 'markdown-mode)
  (with-temp-buffer
    (gfm-mode)
    (insert "| a | b |\n|---|---|\n| 1 | 2 |\n")
    (gfm-pretty-mode 1)
    (goto-char (point-min))
    (let ((hit (gfm-pretty-block-at-point)))
      (should hit)
      (should (eq 'tables (car hit))))))

;;; gfm-pretty-blockquotes tests

(require 'gfm-pretty-blockquotes)

(defun lang-markdown-tests--blockquotes-display-overlays ()
  "Return display overlays tagged by the blockquotes decorator."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'gfm-pretty-blockquotes-revealable))
   (overlays-in (point-min) (point-max))))

(defun lang-markdown-tests--blockquotes-anchor-overlays ()
  "Return anchor overlays tagged by the blockquotes decorator."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'gfm-pretty-blockquotes-anchor))
   (overlays-in (point-min) (point-max))))

(defun lang-markdown-tests--blockquotes-wrap-prefix (n)
  "Build the expected inset+rail+space wrap-prefix for inset width N."
  (gfm-pretty-blockquotes--wrap-prefix-string n))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-inset-cols-default-tracks-tab-width ()
  "Default defcustom value resolves to current `tab-width'."
  (with-temp-buffer
    (setq-local tab-width 5)
    (let ((gfm-pretty-blockquotes-inset-cols 'tab-width))
      (should (= 5 (gfm-pretty-blockquotes--inset-cols))))
    (let ((gfm-pretty-blockquotes-inset-cols 2))
      (should (= 2 (gfm-pretty-blockquotes--inset-cols))))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-anchor-before-string-is-gutter ()
  "Anchor `before-string' is INSET-COLS default-face spaces."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> hello\n")
      (gfm-pretty-mode 1)
      (let* ((anchors (lang-markdown-tests--blockquotes-anchor-overlays))
             (bs (overlay-get (car anchors) 'before-string)))
        (should (stringp bs))
        (should (= 4 (length bs)))
        (should (equal "    " (substring-no-properties bs)))
        (should (eq 'default (get-text-property 0 'face bs))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-wrap-prefix-3-segments ()
  "Anchor `wrap-prefix' is the inset + `▌' + space string in rail-face."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> hello\n")
      (gfm-pretty-mode 1)
      (let* ((anchors (lang-markdown-tests--blockquotes-anchor-overlays))
             (wp (overlay-get (car anchors) 'wrap-prefix)))
        (should (stringp wp))
        (should (= 6 (length wp)))
        (should (equal (substring wp 0 4) "    "))
        (should (equal (substring wp 4 5) "▌"))
        (should (equal (substring wp 5 6) " "))
        (should (eq 'default (get-text-property 0 'face wp)))
        (should (eq 'gfm-pretty-blockquotes-rail-face
                    (get-text-property 4 'face wp))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-plain-renders-rail-prefix-and-wrap ()
  "A plain `> body' line gets a `▌<space>' display and an inset wrap-prefix."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> Pain: clutter\n")
      (gfm-pretty-mode 1)
      (let* ((displays (lang-markdown-tests--blockquotes-display-overlays))
             (anchors  (lang-markdown-tests--blockquotes-anchor-overlays))
             (expected-wrap (lang-markdown-tests--blockquotes-wrap-prefix 4))
             (expected-display (gfm-pretty-blockquotes--rail-string)))
        (should (= 1 (length displays)))
        (let ((ov (car displays)))
          (should (equal expected-display (overlay-get ov 'display)))
          (should (eq 'rail-prefix
                      (overlay-get ov 'gfm-pretty-blockquotes-kind)))
          (should (= 2 (- (overlay-end ov) (overlay-start ov)))))
        (should (= 1 (length anchors)))
        (let* ((ov (car anchors))
               (wp (overlay-get ov 'wrap-prefix)))
          (should (stringp wp))
          (should (equal expected-wrap wp))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-find-blocks-two-separated-by-blank ()
  "Two `>'-prefixed runs separated by a blank line yield two distinct blocks."
  (with-temp-buffer
    (insert "> first\n\n> second\n")
    (let ((blocks (gfm-pretty-blockquotes--find-blocks)))
      (should (= 2 (length blocks))))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-find-blocks-excludes-callout-run ()
  "A `> [!NOTE]\\n> body' run is excluded from plain-blockquote discovery."
  (with-temp-buffer
    (insert "> [!NOTE]\n> body\n")
    (should-not (gfm-pretty-blockquotes--find-blocks))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-bare-gt-produces-one-char-rail ()
  "A bare `>' continuation line gets a 1-char `▌' display overlay."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> first\n>\n> second\n")
      (gfm-pretty-mode 1)
      (goto-char (point-min))
      (should (re-search-forward (rx bol ">" eol) nil t))
      (let* ((bare-pos (match-beginning 0))
             (ov (cl-find-if
                  (lambda (o)
                    (and (eq (overlay-get o 'gfm-pretty-blockquotes-kind)
                             'rail-prefix)
                         (= (overlay-start o) bare-pos)))
                  (overlays-at bare-pos)))
             (expected (gfm-pretty-blockquotes--rail-string t)))
        (should ov)
        (should (equal expected (overlay-get ov 'display)))
        (should (= 1 (- (overlay-end ov) (overlay-start ov)))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-wrap-prefix-overlay-wins-over-text-prop ()
  "Overlay `wrap-prefix' beats markdown-mode's text-property `wrap-prefix \"> \"'."
  (require 'markdown-mode)
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (gfm-mode)
      (insert "> "
              (make-string 200 ?x)
              "\n")
      (font-lock-ensure)
      (gfm-pretty-mode 1)
      (goto-char (point-min))
      (forward-char 2)
      (let ((wp (get-char-property (point) 'wrap-prefix))
            (expected (lang-markdown-tests--blockquotes-wrap-prefix 4)))
        (should (stringp wp))
        (should (equal expected wp)))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-prefix-display-carries-masked-bare ()
  "Each prefix-display overlay carries paired masked / bare variants."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> Pain: clutter\n")
      (gfm-pretty-mode 1)
      (let* ((ov (car (lang-markdown-tests--blockquotes-display-overlays)))
             (masked (overlay-get ov 'gfm-pretty-display-masked))
             (bare (overlay-get ov 'gfm-pretty-display-bare)))
        (should (stringp masked))
        (should (stringp bare))
        (should (equal masked (gfm-pretty-blockquotes--rail-string))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-region-flips-prefix-to-bare ()
  "Active region over the line flips the prefix display to the bare variant."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> Pain: clutter\n")
      (gfm-pretty-mode 1)
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (setq deactivate-mark nil)
      (activate-mark)
      (setq gfm-pretty--last-selection-bounds nil)
      (gfm-pretty--update-selection)
      (let ((prefix-ov (car (lang-markdown-tests--blockquotes-display-overlays))))
        (should (equal (overlay-get prefix-ov 'display)
                       (overlay-get prefix-ov 'gfm-pretty-display-bare))))
      (deactivate-mark)
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-reveal-swaps-display ()
  "Reveal hides the masked display when point sits on a blockquote line.
The variant walker keeps `gfm-pretty-saved-display' in sync with the
masked / bare variant pair so subsequent reveal-restore picks the
right value."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> Pain: clutter\n")
      (gfm-pretty-mode 1)
      (let* ((displays (lang-markdown-tests--blockquotes-display-overlays))
             (ov (car displays))
             (expected (gfm-pretty-blockquotes--rail-string)))
        (should ov)
        (should (equal expected (overlay-get ov 'display)))
        (goto-char (overlay-start ov))
        (gfm-pretty--reveal)
        (should-not (overlay-get ov 'display))
        (should (stringp (overlay-get ov 'gfm-pretty-saved-display)))
        (goto-char (point-max))
        (gfm-pretty--reveal)
        (should (equal expected (overlay-get ov 'display))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-reveal-preserves-inset ()
  "Reveal nils the prefix display but the anchor's gutter before-string stays.
Without the persistent before-string, exposing the raw `> ' source
would unshift body text by INSET-COLS cols and create the jitter the
user reported."
  (with-temp-buffer
    (let ((gfm-pretty-blockquotes-inset-cols 4))
      (insert "> Pain: clutter\n")
      (gfm-pretty-mode 1)
      (let* ((displays (lang-markdown-tests--blockquotes-display-overlays))
             (anchors (lang-markdown-tests--blockquotes-anchor-overlays))
             (display-ov (car displays))
             (anchor-ov (car anchors)))
        (goto-char (overlay-start display-ov))
        (gfm-pretty--reveal)
        (should-not (overlay-get display-ov 'display))
        (let ((bs (overlay-get anchor-ov 'before-string)))
          (should (stringp bs))
          (should (= 4 (length bs)))))
      (gfm-pretty-mode -1))))

(ert-deftest lang-markdown/gfm-pretty-blockquotes-narrowing-rebuild-converges ()
  "Narrowed rebuild + widen + rebuild converges with a clean widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (insert "> first block line one\n"
            "> first block line two\n"
            "\n"
            "> second block line one\n"
            "> second block line two\n"
            "\n"
            "> third block line one\n"
            "> third block line two\n")
    (gfm-pretty-mode 1)
    (let* ((decorator (gfm-pretty--get 'blockquotes))
           (count-of-overlays
            (lambda ()
              (length
               (cl-remove-if-not
                #'overlay-buffer
                (gfm-pretty--state-get 'blockquotes 'overlays))))))
      (goto-char (point-min))
      (search-forward "second block line one")
      (let ((mid-beg (line-beginning-position))
            (mid-end (save-excursion (forward-line 2) (point))))
        (save-restriction
          (narrow-to-region mid-beg mid-end)
          (gfm-pretty--rebuild decorator))
        (widen)
        (gfm-pretty--rebuild decorator))
      (let ((narrow-then-widen (funcall count-of-overlays)))
        (gfm-pretty--rebuild decorator)
        (should (= narrow-then-widen (funcall count-of-overlays)))))
    (gfm-pretty-mode -1)))

(provide 'gfm-pretty-tests)

;;; gfm-pretty-tests.el ends here
