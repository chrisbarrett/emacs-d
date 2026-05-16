;;; lang-markdown/tests.el --- Tests for lang-markdown module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Markdown language support module.

;;; Code:

(require 'ert)

;; Load module files from this directory
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir))
       (lib-file (expand-file-name "lib.el" module-dir)))
  (condition-case nil
      (progn
        (load lib-file nil 'nomessage)
        (load init-file nil 'nomessage))
    (error nil)))

;;; P1: Opening *.md file activates gfm-mode (not markdown-mode)

(ert-deftest lang-markdown/gfm-mode-remap ()
  "P1: markdown-mode should be remapped to gfm-mode or markdown-ts-mode."
  ;; Skip if init.el didn't load (missing +corelib in batch mode)
  ;; Accept gfm-mode (our config) or markdown-ts-mode (tree-sitter default)
  (should (memq (alist-get 'markdown-mode major-mode-remap-alist)
                '(gfm-mode markdown-ts-mode))))

(ert-deftest lang-markdown/prompt-file-association ()
  "P2: /prompt files should be associated with gfm-mode."
  (let ((entry (cl-find-if (lambda (e)
                             (and (stringp (car e))
                                  (string-match-p "prompt" (car e))))
                           auto-mode-alist)))
    (should (eq (cdr entry) 'gfm-mode))))

;;; P9: 11 tempel snippets available in markdown-mode

(ert-deftest lang-markdown/tempel-snippet-count ()
  "P9: There should be at least 10 snippets in markdown.eld."
  (let ((template-file (expand-file-name "templates/markdown.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        ;; Count top-level forms (snippets start with "(name ...")
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward "^(\\w+" nil t)
            (setq count (1+ count)))
          ;; Should have at least 10 snippets (spec says 11)
          (should (>= count 10)))))))

;;; Shared block-border lib (loaded first so consumer files can `require' it).

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lib-dir (expand-file-name "lib" module-dir))
       (borders-file (expand-file-name "+gfm-block-borders.el" lib-dir)))
  (add-to-list 'load-path lib-dir)
  (when (file-exists-p borders-file)
    (load borders-file nil 'nomessage)))

;;; +gfm-block-borders unit tests

(ert-deftest lang-markdown/gfm-block-borders-simulate-wrap-zero-width-terminates ()
  "`gfm-block-borders--simulate-wrap' returns rather than spinning at width 0."
  (let ((res (with-timeout (1 'timeout)
               (gfm-block-borders--simulate-wrap "hello world" 0))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-block-borders-simulate-wrap-tiny-width-with-prefix-terminates ()
  "`gfm-block-borders--simulate-wrap' terminates when width ≤ cont-prefix-w."
  (let ((res (with-timeout (1 'timeout)
               (gfm-block-borders--simulate-wrap "hello world" 1 2))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-block-borders-simulate-wrap-no-wrap-fits ()
  "Text fitting in WIDTH does not produce wrap positions."
  (let ((res (gfm-block-borders--simulate-wrap "hello" 80)))
    (should (= 5 (car res)))
    (should-not (cdr res))))

(ert-deftest lang-markdown/gfm-block-borders-simulate-wrap-breaks-at-space ()
  "Wrap chooses the last space within the line slice."
  (let ((res (gfm-block-borders--simulate-wrap "hello world foo" 8)))
    ;; First line takes "hello wo" -> wrap at last space (after "hello ").
    (should (consp (cdr res)))))

(ert-deftest lang-markdown/gfm-block-borders-max-line-width-respects-indent ()
  "`gfm-block-borders--max-line-width' subtracts INDENT per line."
  (with-temp-buffer
    (insert "    aaaa\n    aa\n    aaaaaa\n")
    (should (= 6 (gfm-block-borders--max-line-width
                  (point-min) (point-max) 4)))
    (should (= 10 (gfm-block-borders--max-line-width
                   (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-block-borders-region-overlaps-p ()
  (should (gfm-block-borders--region-overlaps-p '(1 . 5) '(3 . 7)))
  (should (gfm-block-borders--region-overlaps-p '(1 . 5) '(5 . 9)))
  (should-not (gfm-block-borders--region-overlaps-p '(1 . 4) '(5 . 9))))

(ert-deftest lang-markdown/gfm-block-borders-in-ranges-p ()
  (should (gfm-block-borders--in-ranges-p 5 '((1 . 4) (5 . 9))))
  (should-not (gfm-block-borders--in-ranges-p 12 '((1 . 4) (5 . 9)))))

(ert-deftest lang-markdown/gfm-block-borders-normalised-border-face-resets-styling ()
  "Normalised face spec resets slant/weight/underline/etc."
  (let ((spec (gfm-block-borders--normalised-border-face 'italic)))
    (should (equal (plist-get spec :inherit) 'italic))
    (should (eq (plist-get spec :slant) 'normal))
    (should (eq (plist-get spec :weight) 'normal))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))))

(ert-deftest lang-markdown/gfm-block-borders-extend-clip-face-is-defface ()
  "`gfm-block-borders-extend-clip-face' is a defface with explicit `:extend nil'.
Regression: an anonymous attribute plist `(:extend nil)' is rejected
by Emacs's face-spec parser as \"Invalid face: :extend\" and the
display engine silently drops it — the leak persists.  Only a
`defface'd face actually clips, so assert the face exists, is a
symbol, and carries an explicit `:extend nil' attribute."
  (should (facep 'gfm-block-borders-extend-clip-face))
  (should (eq nil (face-attribute 'gfm-block-borders-extend-clip-face
                                  :extend nil nil)))
  ;; The anonymous plist that the early design tried is, in fact,
  ;; rejected by Emacs's face parser — pin the regression so any
  ;; future revert to a plist trips here.
  (should-error (face-attribute-merged-with
                 :extend 'unspecified '(:extend nil))
                :type 'error))

;;; Narrowing-resilient shared teardown

(ert-deftest lang-markdown/gfm-block-borders-remove-overlays-full-clear-widens ()
  "`--remove-overlays' with no BEG/END clears overlays outside the narrowing.
Regression: under `+presentation-mode' the buffer is narrowed when a full
rebuild fires; the prior implementation left tagged overlays sitting
outside the restriction (zombies on widen).  See the
fix-gfm-narrowing-safety change."
  :tags '(narrowing-regression)
  (let ((overlays nil))
    (let* ((registry (gfm-block-borders-registry-for
                      'lm-test-tag
                      (make-symbol "lm-test-overlays-1"))))
      (set (gfm-block-borders-registry-overlays-symbol registry) nil)
      (with-temp-buffer
        (insert "first region\n\n--- divider ---\n\nsecond region\n")
        (let ((ov-a (make-overlay 1 5))
              (ov-b (make-overlay 30 35)))
          (overlay-put ov-a 'lm-test-tag t)
          (overlay-put ov-b 'lm-test-tag t)
          (set (gfm-block-borders-registry-overlays-symbol registry)
               (list ov-a ov-b)))
        (narrow-to-region 1 10)
        (gfm-block-borders--remove-overlays registry)
        (widen)
        (setq overlays
              (cl-remove-if-not
               (lambda (ov) (overlay-get ov 'lm-test-tag))
               (overlays-in (point-min) (point-max))))))
    (should-not overlays)))

;;; gfm-callouts tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (callouts-file (expand-file-name "lib/+gfm-callouts.el" module-dir)))
  (when (file-exists-p callouts-file)
    (load callouts-file nil 'nomessage)))

(defun lang-markdown-tests--callout-block (type body)
  "Build a callout source block of TYPE with BODY lines."
  (concat (format "> [!%s]\n" type)
          (mapconcat (lambda (l) (concat "> " l)) body "\n")
          "\n"))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-detects-types ()
  "Each known callout type is detected with its label."
  (dolist (type '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
    (with-temp-buffer
      (insert (lang-markdown-tests--callout-block type '("hello")))
      (let ((blocks (gfm-callouts--find-blocks)))
        (should (= 1 (length blocks)))
        (should (equal type (nth 2 (car blocks))))))))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-multiline ()
  "Block end extends through subsequent blockquote lines."
  (with-temp-buffer
    (insert "> [!IMPORTANT]\n> line one\n> line two\n\nplain\n")
    (let* ((blocks (gfm-callouts--find-blocks))
           (block (car blocks)))
      (should (= 1 (length blocks)))
      (goto-char (nth 1 block))
      (should (string= "> line two"
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-ignores-plain-blockquote ()
  (with-temp-buffer
    (insert "> just a quote\n> with two lines\n")
    (should-not (gfm-callouts--find-blocks))))

(ert-deftest lang-markdown/gfm-callouts-mode-creates-overlays ()
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-callouts))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-callouts-mode-removes-overlays ()
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (gfm-callouts-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-callouts))
                         (overlays-in (point-min) (point-max))))))

(defun lang-markdown-tests--prefix-overlays ()
  "Return all callout prefix overlays in the current buffer."
  (cl-remove-if-not
   (lambda (ov) (overlay-get ov 'gfm-callouts-revealable))
   (overlays-in (point-min) (point-max))))

(ert-deftest lang-markdown/gfm-callouts-prefix-overlays-evaporative ()
  "Marker and body display overlays are evaporative and revealable."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (let ((ovs (lang-markdown-tests--prefix-overlays)))
      (should (= 2 (length ovs)))
      (dolist (ov ovs)
        (should (overlay-get ov 'evaporate))
        (should (overlay-get ov 'gfm-callouts-revealable))
        (should (stringp (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-callouts-marker-covers-whole-line ()
  "Marker overlay covers the full marker line; body overlay covers `> '."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (let* ((ovs (sort (lang-markdown-tests--prefix-overlays)
                      (lambda (a b) (< (overlay-start a)
                                       (overlay-start b)))))
           (marker (nth 0 ovs))
           (body   (nth 1 ovs)))
      ;; `> [!NOTE]' is 9 chars.
      (should (= 9 (- (overlay-end marker) (overlay-start marker))))
      (should (= 2 (- (overlay-end body)   (overlay-start body)))))))

(ert-deftest lang-markdown/gfm-callouts-prefix-display-marker-vs-body ()
  "Marker displays `┌─ TITLE'; body displays the side edge `│ '."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (let* ((ovs (sort (lang-markdown-tests--prefix-overlays)
                      (lambda (a b) (< (overlay-start a)
                                       (overlay-start b)))))
           (marker (nth 0 ovs))
           (body   (nth 1 ovs)))
      (should (string-match-p "┌─ NOTE" (overlay-get marker 'display)))
      (should (string-match-p "\\`│ \\'" (overlay-get body   'display))))))

(ert-deftest lang-markdown/gfm-callouts-overrides-blockquote-italic ()
  "A face overlay inheriting `default' covers the whole callout block."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (should (cl-some (lambda (ov)
                       (let ((f (overlay-get ov 'face)))
                         (and (overlay-get ov 'gfm-callouts)
                              (or (eq f 'default)
                                  (and (listp f)
                                       (eq (plist-get f :inherit) 'default))))))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-callouts-marker-only-callout-renders-bottom ()
  "A callout with no body lines still gets a bottom border."
  (with-temp-buffer
    (insert "> [!NOTE]\n\nplain.\n")
    (gfm-callouts-mode 1)
    (let* ((after (cl-some (lambda (ov) (overlay-get ov 'after-string))
                           (overlays-in (point-min) (point-max)))))
      (should (and after (string-match-p "└" after))))))

(ert-deftest lang-markdown/gfm-callouts-reveal-suppresses-display-at-point ()
  "Moving point onto the prefix temporarily clears its display."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (goto-char (point-min))
    (gfm-callouts--reveal)
    (let ((ov (cl-find-if (lambda (o) (overlay-get o 'gfm-callouts-revealable))
                          (overlays-at (point-min)))))
      (should ov)
      (should-not (overlay-get ov 'display))
      (should (stringp (overlay-get ov 'gfm-callouts-saved-display))))
    ;; Move off the prefix; display restores.
    (goto-char (point-max))
    (gfm-callouts--reveal)
    (let ((ov (cl-find-if (lambda (o) (overlay-get o 'gfm-callouts-revealable))
                          (overlays-at (point-min)))))
      (should ov)
      (should (stringp (overlay-get ov 'display))))))

;;; Block-discovery cache

(ert-deftest lang-markdown/gfm-callouts-find-blocks-cache-eq-no-edit ()
  "Two `gfm-callouts--find-blocks' calls with no edits return `eq' lists."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (let ((a (gfm-callouts--find-blocks))
          (b (gfm-callouts--find-blocks)))
      (should (eq a b)))))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-cache-invalidates-on-edit ()
  "Edits invalidate the callouts blocks cache."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (let ((before (gfm-callouts--find-blocks)))
      (goto-char (point-max))
      (insert "\n> [!TIP]\n> More.\n")
      (let ((after (gfm-callouts--find-blocks)))
        (should-not (eq before after))
        (should (= 2 (length after)))))))

;;; Narrowing-resilient discovery and teardown — callouts

(defun lang-markdown-tests--two-slide-callouts-buffer ()
  "Insert a two-slide buffer with one callout per slide."
  (insert "> [!NOTE]\n> first.\n")
  (insert "\n# slide 2\n\n")
  (insert "> [!TIP]\n> second.\n"))

(ert-deftest lang-markdown/gfm-callouts-narrowed-rebuild-does-not-signal ()
  "Narrowed callout rebuild over a widened cache must not signal."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-callouts-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-callouts--rebuild) t)))))

(ert-deftest lang-markdown/gfm-callouts-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-callouts-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-callouts--rebuild)
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-callouts))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length gfm-callouts--overlays) on-buffer))))))

;;; Cross-narrow partial-block coverage — overlay set converges across paths

(defun lang-markdown-tests--tagged-source-positions (tag)
  "Return sorted (BEG . END) positions of overlays carrying TAG."
  (sort
   (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
           (cl-remove-if-not (lambda (ov) (overlay-get ov tag))
                             (overlays-in (point-min) (point-max))))
   (lambda (a b) (or (< (car a) (car b))
                     (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))

;;; Extend-clip test helpers

(defun lang-markdown-tests--clip-overlay-at (tag pos)
  "Return the TAG-tagged extend-clip overlay covering POS, or nil."
  (cl-find-if
   (lambda (ov)
     (and (overlay-get ov tag)
          (eq (overlay-get ov 'face)
              'gfm-block-borders-extend-clip-face)
          (eql (overlay-get ov 'priority)
               gfm-block-borders--extend-clip-priority)))
   (overlays-at pos)))

(defun lang-markdown-tests--clip-positions (tag)
  "Return sorted (BEG . END) positions of TAG's extend-clip anchors."
  (sort
   (mapcar (lambda (ov) (cons (overlay-start ov) (overlay-end ov)))
           (cl-remove-if-not
            (lambda (ov)
              (and (overlay-get ov tag)
                   (eq (overlay-get ov 'face)
                       'gfm-block-borders-extend-clip-face)
                   (eql (overlay-get ov 'priority)
                        gfm-block-borders--extend-clip-priority)))
            (overlays-in (point-min) (point-max))))
   (lambda (a b) (or (< (car a) (car b))
                     (and (= (car a) (car b)) (< (cdr a) (cdr b)))))))

(ert-deftest lang-markdown/gfm-tables-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild.
Regression net for any future narrowing-scoped optimisation that
re-introduces narrowing-dependent caches/teardown."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-tables-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-tables))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-tables--rebuild)
      (widen)
      (gfm-tables--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions 'gfm-tables))))))

(ert-deftest lang-markdown/gfm-code-fences-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-code-fences-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-code-fences))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-code-fences--rebuild)
      (widen)
      (gfm-code-fences--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-code-fences))))))

(ert-deftest lang-markdown/gfm-callouts-narrow-rebuild-widen-rebuild-converges ()
  "narrow → rebuild → widen → rebuild produces same overlay set as widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-callouts-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-callouts))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-callouts--rebuild)
      (widen)
      (gfm-callouts--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-callouts))))))

;;; Extend-clip — callouts

(ert-deftest lang-markdown/gfm-callouts-extend-clip-covers-body-newline ()
  "The callout extend-clip overlay covers each body line's newline.
At priority `gfm-block-borders--extend-clip-priority' (100), above
`hl-line' (-50) and `region', it suppresses a foreign `:extend t'
overlay face's past-EOL fill."
  (with-temp-buffer
    (insert "> [!NOTE]\n> body line.\n")
    (gfm-callouts-mode 1)
    (let* ((body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (nl (line-end-position))     ; newline char of the body line
           (clip (lang-markdown-tests--clip-overlay-at 'gfm-callouts nl)))
      (should clip)
      (should (= 100 (overlay-get clip 'priority))))))

(ert-deftest lang-markdown/gfm-callouts-extend-clip-narrow-converges ()
  "Callout extend-clip anchors converge across narrow → rebuild → widen → rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-callouts-buffer)
    (gfm-callouts-mode 1)
    (let* ((baseline (lang-markdown-tests--clip-positions 'gfm-callouts))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (should baseline)
      (narrow-to-region (point-min) slide-1-end)
      (gfm-callouts--rebuild)
      (widen)
      (gfm-callouts--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--clip-positions 'gfm-callouts))))))

;;; Box-width sizing

(ert-deftest lang-markdown/gfm-callouts-box-width-clamps-to-narrow-window ()
  "A 100-col-content callout in a 60-col window clamps to 60."
  (let ((buf (generate-new-buffer "*gfm-callouts-narrow-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "> [!NOTE]\n> "
                  (make-string 100 ?x) "\n")
          (gfm-callouts-mode 1)
          (let* ((rhs-overlays
                  (cl-remove-if-not
                   (lambda (o)
                     (eq (overlay-get o 'gfm-callouts-kind) 'body-rhs))
                   (overlays-in (point-min) (point-max)))))
            ;; A body line wider than (window-max-chars-per-line - 4) takes
            ;; the overflow path; pad string ends in `│'.
            (should rhs-overlays)
            (let ((after (overlay-get (car rhs-overlays) 'after-string)))
              (should (stringp after))
              (should (string-match-p "│" after)))))
      (kill-buffer buf))))

;;; Wrapped right-edge alignment (overflow path)

(ert-deftest lang-markdown/gfm-callouts-overflow-line-uses-overflow-after-string ()
  "A 200-col body line wraps and the right-edge `│' lands on the wrapped row.
With wrap-prefix `│ ' (2 cols) and box-width = text-width, the closing
`│' must appear in the right-edge after-string of the non-last body
line (last body line's after-string also carries the bottom border)."
  (let ((buf (generate-new-buffer "*gfm-callouts-overflow-test*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "> [!NOTE]\n> " (make-string 200 ?a)
                  "\n> trailing line.\n")
          (gfm-callouts-mode 1)
          (let* ((rhs-overlays
                  (cl-remove-if-not
                   (lambda (o)
                     (eq (overlay-get o 'gfm-callouts-kind) 'body-rhs))
                   (overlays-in (point-min) (point-max))))
                 ;; First body line (200 a's) takes the overflow path; its
                 ;; right-edge after-string is just the padded `│'.
                 (sorted (sort (copy-sequence rhs-overlays)
                               (lambda (a b)
                                 (< (overlay-start a) (overlay-start b)))))
                 (first-rhs (car sorted))
                 (after (overlay-get first-rhs 'after-string)))
            (should after)
            ;; Right-edge string ends in `│' (no bottom border on this row).
            (should (string-suffix-p "│" after))))
      (kill-buffer buf))))

;;; Per-window display overlays

(ert-deftest lang-markdown/gfm-callouts-per-window-display-overlays ()
  "Buffer in two windows of different widths gets per-window display overlays."
  (let ((buf (generate-new-buffer "*gfm-callouts-multi-window*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-callouts-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-callouts))
                                (overlays-in (point-min) (point-max))))
                     (displays (cl-remove-if-not
                                (lambda (o)
                                  (overlay-get o 'gfm-callouts-display))
                                overlays))
                     (windowed (cl-count-if
                                (lambda (o) (overlay-get o 'window))
                                displays)))
                (should (> (length displays) 0))
                (should (= (length displays) windowed))))
            (delete-window other)))
      (kill-buffer buf))))

;;; Window-state diff reconciliation

(ert-deftest lang-markdown/gfm-callouts-schedule-full-rebuild-noop-when-window-state-unchanged ()
  "Full-rebuild scheduler is a no-op when window state is unchanged."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (when (timerp gfm-callouts--rebuild-timer)
      (cancel-timer gfm-callouts--rebuild-timer))
    (setq gfm-callouts--rebuild-timer nil)
    (gfm-callouts--schedule-full-rebuild)
    (should-not gfm-callouts--rebuild-timer)
    (setq gfm-callouts--last-window-state
          (cons 'forged gfm-callouts--last-window-state))
    (gfm-callouts--schedule-full-rebuild)
    (should (timerp gfm-callouts--rebuild-timer))
    (cancel-timer gfm-callouts--rebuild-timer)))

(ert-deftest lang-markdown/gfm-callouts-reconcile-windows-touches-changed-only ()
  "Reconcile replaces only the resized window's display overlays."
  (let ((buf (generate-new-buffer "*gfm-callouts-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-callouts-mode 1)
              (let* ((displays-for
                      (lambda (w)
                        (cl-remove-if-not
                         (lambda (o)
                           (and (overlay-get o 'gfm-callouts-display)
                                (eq (overlay-get o 'window) w)))
                         gfm-callouts--overlays)))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                (setq gfm-callouts--last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              gfm-callouts--last-window-state))
                (gfm-callouts--rebuild-block-for-window
                 (car (gfm-callouts--collect-blocks)) win-a)
                (let ((a-after (funcall displays-for win-a))
                      (b-after (funcall displays-for win-b)))
                  (should-not (cl-intersection a-before a-after))
                  (should (= (length b-before) (length b-after)))
                  (should (cl-every (lambda (o) (memq o b-after))
                                    b-before)))))
            (delete-window win-b)))
      (kill-buffer buf))))

;;; Visible-first prioritisation

(ert-deftest lang-markdown/gfm-callouts-block-visible-p ()
  "`gfm-callouts--block-visible-p' detects overlap with any window range."
  (let ((block (gfm-callouts--make-block
                :range (cons 100 200) :payload nil)))
    (should (gfm-callouts--block-visible-p block '((50 . 250))))
    (should (gfm-callouts--block-visible-p block '((130 . 180))))
    (should-not (gfm-callouts--block-visible-p block '((1 . 99) (201 . 300))))
    (should-not (gfm-callouts--block-visible-p block nil))))

;;; Scoped post-edit rebuild

(defun gfm-callouts--test-overlay-set ()
  "Return gfm-callouts overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-callouts)
        (puthash ov t set)))
    set))

(ert-deftest lang-markdown/gfm-callouts-scoped-edit-inside-single-block ()
  "Editing inside one callout's body only rebuilds that callout."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n\n> [!TIP]\n> Pizza.\n")
    (gfm-callouts-mode 1)
    (let* ((blocks (gfm-callouts--collect-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (r1 (gfm-callouts--block-range b1))
           (r2 (gfm-callouts--block-range b2))
           (collect (lambda (range)
                      (cl-remove-if-not
                       (lambda (o) (overlay-get o 'gfm-callouts))
                       (overlays-in (car range) (cdr range)))))
           (b2-before (funcall collect r2)))
      ;; Edit inside b1 body (line 2 of b1 — past the marker line).
      (save-excursion
        (goto-char (car r1))
        (forward-line 1)
        (let ((p (1+ (point))))
          (setq gfm-callouts--dirty-region (cons p p))))
      (gfm-callouts--rebuild-scoped)
      (let ((b2-after (funcall collect r2)))
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))))))

(ert-deftest lang-markdown/gfm-callouts-scoped-edit-on-marker-full-rebuild ()
  "Edit on the marker line triggers a full rebuild."
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (let* ((blocks (gfm-callouts--find-blocks))
           (marker-beg (nth 0 (car blocks)))
           (marker-line-end (save-excursion
                              (goto-char marker-beg) (line-end-position)))
           (before (gfm-callouts--test-overlay-set)))
      (setq gfm-callouts--dirty-region
            (cons marker-beg marker-line-end))
      (gfm-callouts--rebuild-scoped)
      (let ((after (gfm-callouts--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-callouts-scoped-edit-adjacent-full-rebuild ()
  "Edit on a line adjacent to a callout triggers full rebuild."
  (with-temp-buffer
    (insert "preamble.\n> [!NOTE]\n> Hello.\nepilogue.\n")
    (gfm-callouts-mode 1)
    (let* ((blocks (gfm-callouts--find-blocks))
           (block-beg (nth 0 (car blocks)))
           (above-beg (save-excursion
                        (goto-char block-beg) (forward-line -1)
                        (line-beginning-position)))
           (above-end (save-excursion
                        (goto-char block-beg) (forward-line -1)
                        (line-end-position)))
           (before (gfm-callouts--test-overlay-set)))
      (setq gfm-callouts--dirty-region (cons above-beg above-end))
      (gfm-callouts--rebuild-scoped)
      (let ((after (gfm-callouts--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-callouts-scoped-edit-outside-blocks-noop ()
  "Edit outside every decorated callout is a no-op."
  (with-temp-buffer
    (insert "intro line.\n\nmore.\n\n> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (let ((before (gfm-callouts--test-overlay-set)))
      (setq gfm-callouts--dirty-region (cons 1 5))
      (gfm-callouts--rebuild-scoped)
      (let ((after (gfm-callouts--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)))))

;;; Integration: hang regression with cursor-intangible-mode active

(ert-deftest lang-markdown/gfm-callouts-coexists-with-cursor-intangible-mode ()
  "Enabling callouts in a buffer with cursor-intangible-mode must not hang.
Reproduces the regression where `forward-line' inside the
overlay-creation loop stalls on cursor-intangible/display props
when both `gfm-callouts-mode' and `gfm-code-fences-mode' are active."
  (with-temp-buffer
    (cursor-intangible-mode 1)
    (dotimes (i 5)
      (insert (format "## Section %d\n\nLead.\n\n> [!IMPORTANT]\n> Body %d line a.\n> Body %d line b.\n\n    indented code line %d\n    second indent line\n\nMore text after.\n\n"
                      i i i i)))
    (let ((res (with-timeout (5 'timeout)
                 (gfm-code-fences-mode 1)
                 (gfm-callouts-mode 1)
                 'ok)))
      (should (eq res 'ok))
      (should (cl-some (lambda (o) (overlay-get o 'gfm-callouts))
                       (overlays-in (point-min) (point-max))))
      (should (cl-some (lambda (o) (overlay-get o 'gfm-code-fences))
                       (overlays-in (point-min) (point-max)))))))

;;; Per-window cursor reveal

(ert-deftest lang-markdown/gfm-callouts-reveal-respects-window-restriction ()
  "Reveal in window A doesn't expose source via window B's overlays."
  (let ((buf (generate-new-buffer "*gfm-callouts-reveal-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "> [!NOTE]\n> Hello.\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-callouts-mode 1)
              (with-selected-window win-a
                (goto-char (point-min))
                (gfm-callouts--reveal))
              (let* ((revealable
                      (cl-remove-if-not
                       (lambda (o)
                         (and (overlay-get o 'gfm-callouts-revealable)
                              (= (overlay-start o) (point-min))))
                       gfm-callouts--overlays))
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

;;; gfm-code-fences tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (fences-file (expand-file-name "lib/+gfm-code-fences.el" module-dir)))
  (when (file-exists-p fences-file)
    (load fences-file nil 'nomessage)))

(ert-deftest lang-markdown/gfm-code-fences-find-block ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (let ((blocks (gfm-code-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should (equal "bash" (nth 4 (car blocks)))))))

(ert-deftest lang-markdown/gfm-code-fences-find-block-no-lang ()
  (with-temp-buffer
    (insert "```\ntext\n```\n")
    (let ((blocks (gfm-code-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should-not (nth 4 (car blocks))))))

(ert-deftest lang-markdown/gfm-code-fences-mode-creates-overlays ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-code-fences))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-code-fences-mode-removes-overlays ()
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (gfm-code-fences-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-code-fences))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-code-fences-enabled-via-gfm-mode-hook ()
  (should (memq 'gfm-code-fences-mode gfm-mode-hook)))

;;; Narrowing-resilient discovery and teardown — code fences

(defun lang-markdown-tests--two-slide-fences-buffer ()
  "Insert a two-slide buffer with one fenced code block per slide."
  (insert "```bash\necho one\n```\n")
  (insert "\n# slide 2\n\n")
  (insert "```bash\necho two\n```\n"))

(ert-deftest lang-markdown/gfm-code-fences-narrowed-rebuild-does-not-signal ()
  "Narrowed rebuild of fences over a widened cache must not signal."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-code-fences-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-code-fences--rebuild) t)))))

(ert-deftest lang-markdown/gfm-code-fences-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-code-fences-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-code-fences--rebuild)
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-code-fences))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length gfm-code-fences--overlays) on-buffer))))))

;;; Extend-clip — code fences

(ert-deftest lang-markdown/gfm-code-fences-extend-clip-covers-body-newline ()
  "The fence extend-clip overlay covers each body line's newline.
Carrying `gfm-block-borders-extend-clip-face' (`:extend nil') at
priority 100, it suppresses the past-EOL fill of a `:extend t'
text-property face such as `diff-added' on a ` ```diff ` body line."
  (with-temp-buffer
    (insert "```diff\n+ added line\n```\n")
    (gfm-code-fences-mode 1)
    (let* ((body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (nl (line-end-position))     ; newline char of the `+' line
           (clip (lang-markdown-tests--clip-overlay-at 'gfm-code-fences nl)))
      (should clip)
      (should (= 100 (overlay-get clip 'priority))))))

(ert-deftest lang-markdown/gfm-code-fences-extend-clip-narrow-converges ()
  "Fence extend-clip anchors converge across narrow → rebuild → widen → rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-fences-buffer)
    (gfm-code-fences-mode 1)
    (let* ((baseline (lang-markdown-tests--clip-positions 'gfm-code-fences))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (should baseline)
      (narrow-to-region (point-min) slide-1-end)
      (gfm-code-fences--rebuild)
      (widen)
      (gfm-code-fences--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--clip-positions
                      'gfm-code-fences))))))

;;; Body background fill — code fences

(defun lang-markdown-tests--fence-body-after-string (body-beg lend)
  "Return the body display overlay's `after-string' for line [BODY-BEG, LEND]."
  (let ((ov (cl-find-if
             (lambda (o)
               (eq (overlay-get o 'gfm-code-fences-kind) 'body))
             (overlays-in body-beg (1+ lend)))))
    (and ov (overlay-get ov 'after-string))))

(ert-deftest lang-markdown/gfm-code-fences-diff-body-bg-fills-gap ()
  "A `+' body line's right-edge padding carries the `:extend t' background."
  (with-temp-buffer
    (insert "```diff\n+ x\n```\n")
    (gfm-code-fences-mode 1)
    ;; Stand in for native fontification copying diff-mode's `:extend t'
    ;; `diff-added' face onto the `+' body line — an explicit plist,
    ;; since batch Emacs does not realise a defface's `:extend'.
    (let* ((diff-added-face '(:background "#c3ebc1" :extend t))
           (body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (lend (line-end-position)))
      (put-text-property body-beg (1+ lend) 'face diff-added-face)
      ;; Rebuild so the display pass reads the freshly-applied face.
      (gfm-code-fences--rebuild)
      (let ((after (lang-markdown-tests--fence-body-after-string
                    body-beg lend)))
        (should after)
        ;; The padding (first char of the after-string) is painted with
        ;; the diff background.
        (should (equal "#c3ebc1"
                       (plist-get (get-text-property 0 'face after)
                                  :background)))))))

(ert-deftest lang-markdown/gfm-code-fences-plain-body-no-bg-fill ()
  "A body line with no `:extend t' background keeps the border-face padding."
  (with-temp-buffer
    (insert "```text\nplain line\n```\n")
    (gfm-code-fences-mode 1)
    (let* ((body-beg (progn (goto-char (point-min))
                            (forward-line 1) (point)))
           (lend (line-end-position))
           (after (lang-markdown-tests--fence-body-after-string
                   body-beg lend)))
      (should after)
      (should-not (plist-get (get-text-property 0 'face after)
                             :background)))))

(ert-deftest lang-markdown/gfm-code-fences-line-extend-bg-ignores-non-extending ()
  "`gfm-code-fences--line-extend-bg' ignores a `:background' without `:extend t'."
  (with-temp-buffer
    (insert "code line\n")
    ;; A `:background' without `:extend t' must not fill the gap.
    (put-text-property (point-min) (1- (point-max))
                       'face '(:background "#abcdef"))
    (should-not (gfm-code-fences--line-extend-bg (point-min)
                                                 (1- (point-max))))
    ;; A face specifying both is picked up.
    (put-text-property (point-min) (1- (point-max))
                       'face '(:background "#abcdef" :extend t))
    (should (equal "#abcdef"
                   (gfm-code-fences--line-extend-bg (point-min)
                                                    (1- (point-max)))))))

(ert-deftest lang-markdown/gfm-code-fences-border-face-resets-styling ()
  "Border face spec inherits the configured face but resets styling
attrs that would otherwise leak from font-lock onto box edges (slant,
weight, underline, overline, strike-through, box)."
  (let ((spec (gfm-code-fences--normalised-border-face 'italic)))
    (should (equal (plist-get spec :inherit) 'italic))
    (should (eq (plist-get spec :slant) 'normal))
    (should (eq (plist-get spec :weight) 'normal))
    (should (null (plist-get spec :underline)))
    (should (null (plist-get spec :overline)))
    (should (null (plist-get spec :strike-through)))
    (should (null (plist-get spec :box)))))

(ert-deftest lang-markdown/gfm-code-fences-find-indent-block ()
  (with-temp-buffer
    (insert "Para.\n\n    code one\n    code two\n\nMore.\n")
    (let ((blocks (gfm-code-fences--find-indent-blocks nil)))
      (should (= 1 (length blocks)))
      (should (= 4 (nth 2 (car blocks)))))))

(ert-deftest lang-markdown/gfm-code-fences-yaml-mode-prefers-treesit ()
  "Helmet language mode picks yaml-ts-mode when grammar available."
  (should (eq 'yaml-ts-mode (gfm-code-fences--yaml-mode))))

(ert-deftest lang-markdown/gfm-code-fences-yaml-helmet-fontifies-body ()
  "YAML helmet body receives face overlays from the chosen yaml mode."
  (with-temp-buffer
    (insert "---\nkey: value\n---\nbody\n")
    (gfm-code-fences-mode 1)
    (goto-char (point-min))
    (search-forward "key")
    (let ((pos (match-beginning 0)))
      (should (cl-some (lambda (ov)
                         (and (overlay-get ov 'gfm-code-fences)
                              (overlay-get ov 'face)))
                       (overlays-at pos))))))

(ert-deftest lang-markdown/gfm-code-fences-yaml-helmet-empty-body-noerror ()
  "Empty YAML helmet body does not error during rebuild."
  (with-temp-buffer
    (insert "---\n---\nbody\n")
    (should (progn (gfm-code-fences-mode 1) t))))

(ert-deftest lang-markdown/gfm-code-fences-skip-indent-inside-fence ()
  (with-temp-buffer
    (insert "```\n    looks indented\n```\n")
    (let* ((fenced (gfm-code-fences--find-blocks))
           (excluded (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b))) fenced))
           (indents (gfm-code-fences--find-indent-blocks excluded)))
      (should (= 0 (length indents))))))

;;; Language → mode mapping

(ert-deftest lang-markdown/gfm-code-fences-lang-mode-aliases ()
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
    (should (eq (cdr case) (gfm-code-fences--lang-mode (car case))))))

(ert-deftest lang-markdown/gfm-code-fences-lang-mode-case-insensitive ()
  "Language tag lookup is case-insensitive."
  (should (eq 'python-mode (gfm-code-fences--lang-mode "Python")))
  (should (eq 'csharp-mode (gfm-code-fences--lang-mode "C#")))
  (should (eq 'js-mode     (gfm-code-fences--lang-mode "JavaScript"))))

(ert-deftest lang-markdown/gfm-code-fences-lang-mode-fallback ()
  "Unknown languages fall back to <lang>-mode."
  (should (eq 'totally-made-up-mode
              (gfm-code-fences--lang-mode "totally-made-up"))))

(ert-deftest lang-markdown/gfm-code-fences-icon-for-aliased-lang ()
  "Icon lookup returns a language-specific glyph (not the fallback)."
  (let ((fallback (nerd-icons-icon-for-mode 'fundamental-mode)))
    (dolist (lang '("py" "rb" "javascript" "zsh" "cs" "cpp"))
      (let ((icon (gfm-code-fences--icon-for-lang lang)))
        (should (and (stringp icon) (> (length icon) 0)))
        (should-not (equal icon fallback))))))

;;; Wrap simulation termination guard

(ert-deftest lang-markdown/gfm-code-fences-simulate-wrap-zero-width-terminates ()
  "`gfm-code-fences--simulate-wrap' returns rather than spinning at width 0."
  (let ((res (with-timeout (1 'timeout)
               (gfm-code-fences--simulate-wrap "hello world" 0))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

(ert-deftest lang-markdown/gfm-code-fences-simulate-wrap-tiny-width-with-prefix-terminates ()
  "`gfm-code-fences--simulate-wrap' terminates when width ≤ cont-prefix-w."
  (let ((res (with-timeout (1 'timeout)
               (gfm-code-fences--simulate-wrap "hello world" 1 2))))
    (should (consp res))
    (should (not (eq res 'timeout)))))

;;; Discovery cache

(ert-deftest lang-markdown/gfm-code-fences-find-blocks-cache-eq-no-edit ()
  "Two `gfm-code-fences--find-blocks' calls with no edits return `eq' lists."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (let ((a (gfm-code-fences--find-blocks))
          (b (gfm-code-fences--find-blocks)))
      (should (eq a b)))))

(ert-deftest lang-markdown/gfm-code-fences-find-blocks-cache-invalidates-on-edit ()
  "Edits invalidate the fenced-blocks cache."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (let ((before (gfm-code-fences--find-blocks)))
      (goto-char (point-max))
      (insert "\n```sh\nfoo\n```\n")
      (let ((after (gfm-code-fences--find-blocks)))
        (should-not (eq before after))
        (should (= 2 (length after)))))))

(ert-deftest lang-markdown/gfm-code-fences-yaml-helmet-cache-invalidates-on-edit ()
  "Edits invalidate the YAML-helmet cache."
  (with-temp-buffer
    (insert "---\nkey: value\n---\nbody\n")
    (let ((before (gfm-code-fences--find-yaml-helmet)))
      (should before)
      (let ((same (gfm-code-fences--find-yaml-helmet)))
        (should (eq before same)))
      (goto-char (point-max))
      (insert "more\n")
      (let ((after (gfm-code-fences--find-yaml-helmet)))
        ;; Helmet positions unchanged but cache list itself is fresh.
        (should-not (eq before after))))))

(ert-deftest lang-markdown/gfm-code-fences-indent-blocks-cache-eq-no-edit ()
  "Two `gfm-code-fences--find-indent-blocks' calls return `eq' lists."
  (with-temp-buffer
    (insert "Para.\n\n    code one\n    code two\n\nMore.\n")
    (let ((a (gfm-code-fences--find-indent-blocks nil))
          (b (gfm-code-fences--find-indent-blocks nil)))
      (should (eq a b)))))

;;; Per-window display overlays

(ert-deftest lang-markdown/gfm-code-fences-per-window-display-overlays ()
  "Buffer in two windows of different widths gets per-window display overlays.
Anchors stay shared across windows; only display overlays carry a
`window' restriction."
  (let ((buf (generate-new-buffer "*gfm-code-fences-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "```bash\necho hi\n```\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-code-fences-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-code-fences))
                                (overlays-in (point-min) (point-max))))
                     (displays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-code-fences-display))
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

(ert-deftest lang-markdown/gfm-code-fences-schedule-full-rebuild-noop-when-window-state-unchanged ()
  "Full-rebuild scheduler is a no-op when window state is unchanged."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (when (timerp gfm-code-fences--rebuild-timer)
      (cancel-timer gfm-code-fences--rebuild-timer))
    (setq gfm-code-fences--rebuild-timer nil)
    (gfm-code-fences--schedule-full-rebuild)
    (should-not gfm-code-fences--rebuild-timer)
    ;; Forge a state change → timer armed.
    (setq gfm-code-fences--last-window-state
          (cons 'forged gfm-code-fences--last-window-state))
    (gfm-code-fences--schedule-full-rebuild)
    (should (timerp gfm-code-fences--rebuild-timer))
    (cancel-timer gfm-code-fences--rebuild-timer)))

(ert-deftest lang-markdown/gfm-code-fences-reconcile-windows-touches-changed-only ()
  "Reconciling windows replaces only the resized window's display overlays."
  (let ((buf (generate-new-buffer "*gfm-code-fences-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "```bash\necho hi\n```\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-code-fences-mode 1)
              (let* ((displays-for
                      (lambda (w)
                        (cl-remove-if-not
                         (lambda (o)
                           (and (overlay-get o 'gfm-code-fences-display)
                                (eq (overlay-get o 'window) w)))
                         gfm-code-fences--overlays)))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                ;; Forge a width change for win-a only.
                (setq gfm-code-fences--last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              gfm-code-fences--last-window-state))
                ;; Drive the deferred rebuild path synchronously by calling
                ;; the per-window helper directly: reconcile schedules
                ;; rebuilds via idle timers, but ERT tests can't easily wait
                ;; for them to fire.  Helper does the same work that the
                ;; idle callback eventually does.
                (gfm-code-fences--rebuild-block-for-window
                 (car (gfm-code-fences--collect-blocks)) win-a)
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

(defun gfm-code-fences--test-overlay-set ()
  "Return gfm-code-fences overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-code-fences)
        (puthash ov t set)))
    set))

(ert-deftest lang-markdown/gfm-code-fences-scoped-edit-inside-single-block ()
  "Editing inside one fenced block only rebuilds that block's overlays."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n\n```sh\nfoo\n```\n")
    (gfm-code-fences-mode 1)
    (let* ((blocks (gfm-code-fences--collect-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (r1 (gfm-code-fences--block-range b1))
           (r2 (gfm-code-fences--block-range b2))
           (collect (lambda (range)
                      (cl-remove-if-not
                       (lambda (o) (overlay-get o 'gfm-code-fences))
                       (overlays-in (car range) (cdr range)))))
           (b2-before (funcall collect r2)))
      ;; Edit a body line inside b1 only, away from any fence line.
      (save-excursion
        (goto-char (car r1))
        (forward-line 1)
        (let ((p (point)))
          (setq gfm-code-fences--dirty-region
                (cons (1+ p) (1+ p)))))
      (gfm-code-fences--rebuild-scoped)
      (let ((b2-after (funcall collect r2)))
        ;; b2's overlay objects survived `eq'.
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))))))

(ert-deftest lang-markdown/gfm-code-fences-scoped-edit-on-fence-boundary-full-rebuild ()
  "Edit overlapping a fence opening line triggers a full rebuild."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (let* ((blocks (gfm-code-fences--find-blocks))
           (open-beg (nth 0 (car blocks)))
           (open-line-beg (save-excursion
                            (goto-char open-beg) (line-beginning-position)))
           (open-line-end (save-excursion
                            (goto-char open-beg) (line-end-position)))
           (before (gfm-code-fences--test-overlay-set)))
      (setq gfm-code-fences--dirty-region (cons open-line-beg open-line-end))
      (gfm-code-fences--rebuild-scoped)
      (let ((after (gfm-code-fences--test-overlay-set)))
        ;; Full rebuild → original overlay objects no longer present.
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-code-fences-scoped-edit-blank-adjacent-indent-full-rebuild ()
  "Edit on a blank line adjacent to an indent block triggers full rebuild."
  (with-temp-buffer
    (insert "Para.\n\n    code line\n    next code\n\nMore.\n")
    (gfm-code-fences-mode 1)
    (let* ((blocks (gfm-code-fences--find-indent-blocks nil))
           (block (car blocks))
           (beg (nth 0 block))
           (blank-beg (save-excursion
                        (goto-char beg) (forward-line -1)
                        (line-beginning-position)))
           (blank-end (save-excursion
                        (goto-char beg) (forward-line -1)
                        (line-end-position)))
           (before (gfm-code-fences--test-overlay-set)))
      (setq gfm-code-fences--dirty-region (cons blank-beg blank-end))
      (gfm-code-fences--rebuild-scoped)
      (let ((after (gfm-code-fences--test-overlay-set)))
        (let (xs)
          (maphash (lambda (k _) (push k xs)) before)
          (should (cl-every (lambda (ov) (not (gethash ov after))) xs)))))))

(ert-deftest lang-markdown/gfm-code-fences-scoped-edit-outside-blocks-noop ()
  "Edit outside every decorated block is a no-op."
  (with-temp-buffer
    (insert "intro line\n\n```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (let ((before (gfm-code-fences--test-overlay-set))
          (rebuild-count (alist-get 'rebuild-count gfm-code-fences--stats)))
      (setq gfm-code-fences--dirty-region (cons 1 5))
      (gfm-code-fences--rebuild-scoped)
      (let ((after (gfm-code-fences--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)
        (should (= rebuild-count
                   (alist-get 'rebuild-count gfm-code-fences--stats)))))))

;;; Visible-first prioritisation

(ert-deftest lang-markdown/gfm-code-fences-block-visible-p ()
  "`gfm-code-fences--block-visible-p' detects overlap with any window range."
  (let ((block (gfm-code-fences--make-block
                :kind 'fenced :range (cons 100 200) :payload nil)))
    (should (gfm-code-fences--block-visible-p block '((50 . 250))))
    (should (gfm-code-fences--block-visible-p block '((130 . 180))))
    (should (gfm-code-fences--block-visible-p block '((50 . 100))))
    (should-not (gfm-code-fences--block-visible-p block '((1 . 99) (201 . 300))))
    (should-not (gfm-code-fences--block-visible-p block nil))
    (should (gfm-code-fences--block-visible-p block '((1 . 50) (130 . 180))))))

;;; Performance instrumentation

(ert-deftest lang-markdown/gfm-code-fences-stats-increment-across-rebuilds ()
  "Stats accumulate rebuild count and total time across rebuilds."
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (let ((before (alist-get 'rebuild-count gfm-code-fences--stats)))
      (gfm-code-fences--rebuild)
      (let ((after (alist-get 'rebuild-count gfm-code-fences--stats)))
        (should (> after before))))))

(ert-deftest lang-markdown/gfm-code-fences-phase-totals-include-required-keys ()
  "Phase totals include the six keys required by the spec."
  (with-temp-buffer
    (insert "---\nk: v\n---\n```bash\necho hi\n```\n\nPara.\n\n    indent\n")
    (gfm-code-fences-mode 1)
    (let ((phases (alist-get 'phase-totals gfm-code-fences--stats)))
      (dolist (k '(find-fenced find-yaml find-indent
                   compose-borders compose-overflow apply))
        (should (assq k phases))))))

;;; gfm-tables tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tables-file (expand-file-name "lib/+gfm-tables.el" module-dir)))
  (when (file-exists-p tables-file)
    (load tables-file nil 'nomessage)))

;;; Cell parser

(ert-deftest lang-markdown/gfm-tables-split-row-simple ()
  (should (equal '("a" "b" "c")
                 (gfm-tables--split-row "| a | b | c |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-escaped-pipe ()
  (should (equal '("a | b" "c")
                 (gfm-tables--split-row "| a \\| b | c |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-single-tick-code ()
  (should (equal '("a" "`b|c`" "d")
                 (gfm-tables--split-row "| a | `b|c` | d |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-double-tick-code ()
  (should (equal '("a" "``b|c``" "d")
                 (gfm-tables--split-row "| a | ``b|c`` | d |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-unbalanced-tick ()
  "Unbalanced backtick is treated as literal text."
  (should (equal '("a" "`b" "c")
                 (gfm-tables--split-row "| a | `b | c |"))))

;;; Block discovery

(ert-deftest lang-markdown/gfm-tables-find-blocks-standard ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (let ((blocks (gfm-tables--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest lang-markdown/gfm-tables-find-blocks-rejects-lone-delim ()
  (with-temp-buffer
    (insert "Some prose.\n| - | - |\nMore prose.\n")
    (should-not (gfm-tables--find-blocks))))

(ert-deftest lang-markdown/gfm-tables-find-blocks-cache-invalidates-on-edit ()
  "`gfm-tables--find-blocks' caches by `buffer-modified-tick'.
Two calls without an intervening edit return `eq' lists; an edit
invalidates the cache and the next call returns a fresh list reflecting
the new buffer state."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\nintro\n")
    (let ((first (gfm-tables--find-blocks))
          (second (gfm-tables--find-blocks)))
      (should (eq first second)))
    ;; Append a second table; cache must invalidate.
    (goto-char (point-max))
    (insert "\n| C | D |\n| - | - |\n| 3 | 4 |\n")
    (let ((after (gfm-tables--find-blocks)))
      (should (= 2 (length after))))))

(ert-deftest lang-markdown/gfm-tables-find-blocks-skips-fenced ()
  (with-temp-buffer
    (insert "```\n| A | B |\n| - | - |\n| 1 | 2 |\n```\n")
    (let* ((fenced (gfm-code-fences--find-blocks))
           (excluded (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b))) fenced))
           (blocks (gfm-tables--find-blocks excluded)))
      (should (= 0 (length blocks))))))

;;; Column widths

(ert-deftest lang-markdown/gfm-tables-column-widths-unaligned ()
  (let* ((rows '(("Header" "B")
                 ("a" "longer")
                 ("xx" "y")))
         (widths (gfm-tables--column-widths rows)))
    (should (equal 6 (aref widths 0)))
    (should (equal 6 (aref widths 1)))))

(ert-deftest lang-markdown/gfm-tables-box-width ()
  ;; 2 cols, widths 3 and 5 → 2 + (5) + (7) + 1 = 15
  (should (= 15 (gfm-tables--box-width (vector 3 5)))))

;;; Cell wrapping

(ert-deftest lang-markdown/gfm-tables-cell-tokens-splits-on-whitespace ()
  (should (equal '("foo" "bar" "baz")
                 (gfm-tables--cell-tokens "  foo  bar baz "))))

(ert-deftest lang-markdown/gfm-tables-cell-tokens-empty-string ()
  (should (equal '() (gfm-tables--cell-tokens "")))
  (should (equal '() (gfm-tables--cell-tokens "   "))))

(ert-deftest lang-markdown/gfm-tables-cell-tokens-preserves-properties ()
  (let* ((src (concat "abc " (propertize "def" 'face 'bold)))
         (tokens (gfm-tables--cell-tokens src)))
    (should (equal '("abc" "def") tokens))
    (should (eq 'bold (get-text-property 0 'face (cadr tokens))))))

(ert-deftest lang-markdown/gfm-tables-slice-by-visible-width-basic ()
  (should (equal '("abc" "de") (gfm-tables--slice-by-visible-width "abcde" 3))))

(ert-deftest lang-markdown/gfm-tables-slice-by-visible-width-zero-falls-back ()
  ;; Width 0 must still make progress (one char per slice).
  (let ((chunks (gfm-tables--slice-by-visible-width "ab" 0)))
    (should (= 2 (length chunks)))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-no-wrap ()
  (should (equal '("hello") (gfm-tables--wrap-cell "hello" 10))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-word-boundary ()
  (should (equal '("the quick" "brown fox")
                 (gfm-tables--wrap-cell "the quick brown fox" 9))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-hard-break-long-word ()
  (let ((lines (gfm-tables--wrap-cell "abcdefghij" 4)))
    (should (cl-every (lambda (l) (<= (string-width l) 4)) lines))
    (should (equal "abcdefghij" (apply #'concat lines)))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-empty-returns-one-empty-line ()
  (should (equal '("") (gfm-tables--wrap-cell "" 5))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-preserves-properties ()
  (let* ((src (concat (propertize "abc" 'face 'bold) " def"))
         (lines (gfm-tables--wrap-cell src 3)))
    (should (eq 'bold (get-text-property 0 'face (car lines))))))

;;; Width fitting

(ert-deftest lang-markdown/gfm-tables-fit-widths-under-budget-passthrough ()
  (should (equal (vector 3 5) (gfm-tables--fit-widths (vector 3 5) 100))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-caps-widest ()
  ;; natural sum 30, budget 20: smaller col fits naturally, widest capped.
  (let ((fitted (gfm-tables--fit-widths (vector 5 25) 20)))
    (should (= 5 (aref fitted 0)))
    (should (= 15 (aref fitted 1)))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-equal-distribution ()
  ;; Equal natural widths over budget → all capped equally.
  (let ((fitted (gfm-tables--fit-widths (vector 20 20) 30)))
    (should (= (aref fitted 0) (aref fitted 1)))
    (should (<= (+ (aref fitted 0) (aref fitted 1)) 30))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-uses-full-budget ()
  "Integer slack from binary search is distributed so sum = budget."
  ;; Natural [15 118 57], budget 50: water cap is 17 → 15+17+17=49.
  ;; The 1 unit of slack must be redistributed so total = 50.
  (let ((fitted (gfm-tables--fit-widths (vector 15 118 57) 50)))
    (should (= 50 (cl-loop for w across fitted sum w)))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-floor-at-1 ()
  ;; Tiny budget shouldn't produce zero widths.
  (let ((fitted (gfm-tables--fit-widths (vector 10 10 10) 1)))
    (should (cl-every (lambda (w) (>= w 1)) (cl-coerce fitted 'list)))))

;;; Multi-line compose

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-single-line ()
  "If no cell exceeds its width, output is a single line (no `\\n')."
  (let ((row (gfm-tables--compose-multiline-row '("a" "b") (vector 1 1)
                                                'body-default)))
    (should-not (string-match-p "\n" row))))

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-wraps-cell ()
  "A cell wider than its column wraps to multiple visual lines."
  (let* ((row (gfm-tables--compose-multiline-row
               '("a" "one two three four") (vector 1 9)
               'body-default))
         (lines (split-string row "\n")))
    (should (>= (length lines) 2))
    (should (cl-every (lambda (l) (= (length l) (length (car lines)))) lines))))

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-pads-short-cells ()
  "Short cells get padded with blank lines so columns stay aligned."
  (let* ((row (gfm-tables--compose-multiline-row
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

(ert-deftest lang-markdown/gfm-tables-fontify-cell-applies-bold-face ()
  "Bold markdown inside a cell receives `markdown-bold-face'."
  (let ((s (gfm-tables--fontify-cell "**bold**")))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face s)))
                         (or (eq f 'markdown-bold-face)
                             (and (listp f) (memq 'markdown-bold-face f)))))
                     (number-sequence 0 (1- (length s)))))))

(ert-deftest lang-markdown/gfm-tables-fontify-cell-applies-code-face ()
  "Inline code inside a cell receives `markdown-inline-code-face'."
  (let ((s (gfm-tables--fontify-cell "`code`")))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face s)))
                         (or (eq f 'markdown-inline-code-face)
                             (and (listp f)
                                  (memq 'markdown-inline-code-face f)))))
                     (number-sequence 0 (1- (length s)))))))

(ert-deftest lang-markdown/gfm-tables-fontify-cell-preserves-width ()
  "Fontified cell has the same visible width as the raw cell when
`markdown-hide-markup' is off — i.e. markup chars remain on screen."
  (with-temp-buffer
    (setq buffer-invisibility-spec '(t))
    (dolist (raw '("plain" "**bold**" "*it*" "`code`" "[t](u)" ""))
      (let ((fontified (gfm-tables--fontify-cell raw)))
        (should (= (string-width raw)
                   (gfm-tables--visible-width fontified)))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-honours-display ()
  "`display' string property changes visible width."
  (let ((s (concat "abc" (propertize "X" 'display "longer") "de")))
    (should (= (+ 5 (string-width "longer"))
               (gfm-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-tables-visible-width-honours-invisibility-spec ()
  "Invisible-tagged chars only shrink width when in `buffer-invisibility-spec'."
  (let ((s (concat "ab" (propertize "XX" 'invisible 'tag) "cd")))
    (with-temp-buffer
      (setq buffer-invisibility-spec nil)
      (should (= 6 (gfm-tables--visible-width s)))
      (setq buffer-invisibility-spec '(tag))
      (should (= 4 (gfm-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-honours-composition ()
  "`composition' property compresses visible width to that of the composed glyph.
This is what `markdown-mode' uses to hide URLs when `markdown-hide-urls'
is non-nil — `(url)' is composed into a single chain glyph."
  (let ((s (copy-sequence "abcXXXXXde")))
    (compose-string s 3 8 ?Y)
    (should (= 6 (gfm-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-tables-visible-width-link-with-hidden-url ()
  "Fontified link cell with `markdown-hide-urls' on reports the visible-only width."
  (let ((prev (default-value 'markdown-hide-urls)))
    (unwind-protect
        (progn
          (setq-default markdown-hide-urls t)
          ;; Drop any cached fontify buffer so the new default takes effect.
          (when (get-buffer " *gfm-tables-fontify*")
            (kill-buffer " *gfm-tables-fontify*"))
          (let ((s (gfm-tables--fontify-cell
                    "[label](https://example.com/very/long/path)")))
            (should (<= (gfm-tables--visible-width s)
                        (+ (length "label") 3)))))
      (setq-default markdown-hide-urls prev)
      (when (get-buffer " *gfm-tables-fontify*")
        (kill-buffer " *gfm-tables-fontify*")))))

(ert-deftest lang-markdown/gfm-tables-visible-width-compute-walks-source-overlays ()
  "With (buffer beg end) args, the walker honours overlay display strings.
An overlay carrying a `display' string in the source buffer is counted
at the display width, not the underlying text width."
  (with-temp-buffer
    (insert "hello world")
    (let ((ov (make-overlay 1 6)))           ; covers "hello"
      (overlay-put ov 'display "X"))
    ;; Region [1, 12) is "hello world": "hello" -> "X" (1) + " world" (6) = 7.
    (should (= 7 (gfm-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-compute-honours-source-invisible ()
  "With source args, an overlay `invisible' prop in `buffer-invisibility-spec'
shrinks the measured width."
  (with-temp-buffer
    (insert "abcdef")
    (setq buffer-invisibility-spec '(tag))
    (let ((ov (make-overlay 3 5)))           ; covers "cd"
      (overlay-put ov 'invisible 'tag))
    (should (= 4 (gfm-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-compute-region-trims-padding ()
  "Source-region measurement trims leading/trailing whitespace of the cell."
  (with-temp-buffer
    (insert "  abc  ")
    (should (= 3 (gfm-tables--visible-width--compute
                  (buffer-substring (point-min) (point-max))
                  (current-buffer) (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-nil-region-args-unchanged ()
  "Passing nil for the new region args leaves the string-walk path unchanged."
  (let ((s (concat "abc" (propertize "X" 'display "longer") "de")))
    (should (= (gfm-tables--visible-width s)
               (gfm-tables--visible-width s nil nil nil)))))

(ert-deftest lang-markdown/gfm-tables-cell-link-pos-finds-inline-link ()
  "Inline link inside a cell is locatable from the row overlay."
  (with-temp-buffer
    (gfm-mode)
    (insert "| col |\n| --- |\n| [text](https://example.com) |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (let* ((row-ov (cl-find-if (lambda (o)
                                 (overlay-get o 'gfm-tables-cell-bounds))
                               (overlays-at (line-beginning-position))))
           (pos (gfm-tables--cell-link-pos row-ov 0)))
      (should pos)
      (save-excursion
        (goto-char pos)
        (should (looking-at-p "\\[text\\](https://example\\.com)"))))))

(ert-deftest lang-markdown/gfm-tables-cell-link-pos-no-link-returns-nil ()
  "A cell with no link returns nil from `cell-link-pos'."
  (with-temp-buffer
    (gfm-mode)
    (insert "| col |\n| --- |\n| plain text |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (let ((row-ov (cl-find-if (lambda (o)
                                (overlay-get o 'gfm-tables-cell-bounds))
                              (overlays-at (line-beginning-position)))))
      (should-not (gfm-tables--cell-link-pos row-ov 0)))))

(ert-deftest lang-markdown/gfm-tables-row-char-bounds-aligns-after-composition ()
  "Cell char bounds reflect actual cell string length, not visible width.
A cell with a composition (long source, narrow display) padded to its
column width must still cover all its source chars in the bounds."
  (let* ((cell (copy-sequence "abcXXXXXde"))
         (_ (compose-string cell 3 8 ?Y))
         (bounds (gfm-tables--row-char-bounds (list cell "ok") (vector 6 2))))
    ;; cell 0: 1 (after pipe) → 1 + 2 + 10 (raw len) + 0 (pad) = 13.
    (should (equal (nth 0 bounds) (cons 1 13)))
    ;; gap at 13. cell 1: 14 → 14 + 2 + 2 + 0 = 18.
    (should (equal (nth 1 bounds) (cons 14 18)))))

(ert-deftest lang-markdown/gfm-tables-row-char-bounds-end-matches-compose-row-length ()
  "Last cell's end + 1 (closing pipe) equals `compose-row's string length."
  (let* ((cells '("ab" "cd" "e"))
         (col-widths (vector 2 3 1))
         (s (gfm-tables--compose-row cells col-widths 'body-default))
         (bounds (gfm-tables--row-char-bounds cells col-widths)))
    (should (= (length s) (1+ (cdr (car (last bounds))))))))

(ert-deftest lang-markdown/gfm-tables-multiline-row-char-bounds-per-line ()
  "Per-visual-line bounds reflect the wrapped cell content on each line."
  (let* ((cells '("a" "one two three"))
         (col-widths (vector 1 5))
         (per-line (gfm-tables--multiline-row-char-bounds cells col-widths)))
    ;; Two visual lines (cell 1 wraps).
    (should (>= (length per-line) 2))
    ;; Each line's bounds list has one entry per column.
    (dolist (cb per-line)
      (should (= (length cb) 2)))))

(ert-deftest lang-markdown/gfm-tables-compose-row-preserves-cell-faces ()
  "`compose-row' on body-alt row keeps existing markdown faces on cell text."
  (let* ((cell (gfm-tables--fontify-cell "**bold**"))
         (row (gfm-tables--compose-row (list cell) (vector 8) 'body-alt)))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face row)))
                         (and (listp f) (memq 'markdown-bold-face f))))
                     (number-sequence 0 (1- (length row)))))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face row)))
                         (and (listp f)
                              (memq 'gfm-tables-row-alt-face f))))
                     (number-sequence 0 (1- (length row)))))))

;;; Overlay lifetime

(ert-deftest lang-markdown/gfm-tables-overlays-not-evaporative ()
  "Table overlays must not evaporate when their region empties."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-tables)
        (should-not (overlay-get ov 'evaporate))))))

;;; Indirect editing

(ert-deftest lang-markdown/gfm-tables-block-at-point-inside ()
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (search-forward "1")
    (let ((bounds (gfm-tables--block-at-point)))
      (should bounds)
      (should (< (car bounds) (point)))
      (should (> (cdr bounds) (point))))))

(ert-deftest lang-markdown/gfm-tables-block-at-point-outside-returns-nil ()
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (should-not (gfm-tables--block-at-point))))

(ert-deftest lang-markdown/gfm-tables-edit-table-command-defined ()
  (should (commandp 'gfm-tables-edit-table-at-point)))

(ert-deftest lang-markdown/gfm-tables-edit-cell-command-defined ()
  (should (commandp 'gfm-tables-edit-cell-at-point)))

(ert-deftest lang-markdown/gfm-tables-edit-cell-no-spurious-newline ()
  "Committing a cell edit with no changes must not split the row.
`markdown--edit-indirect-after-commit-function' appends \\n to the
committed region, treating it as a code block.  For a cell edit the
region is just cell content; the trailing newline must be stripped."
  (with-temp-buffer
    (gfm-mode)
    (insert "| A             | B   |\n| ------------- | --- |\n"
            "| `(parameter)` | foo |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (forward-line 2)
    (forward-char 4)
    (let* ((info (gfm-tables--cell-info-at-point))
           (row-ov (car info))
           (idx (cdr info))
           (bounds (gfm-tables--cell-content-bounds row-ov idx))
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
                  #'gfm-tables--cell-edit-mark-pending nil t)
        (add-hook 'edit-indirect-before-commit-hook
                  #'gfm-tables--cell-edit-sanitise nil t))
      (with-current-buffer src-buf
        (add-hook 'edit-indirect-after-commit-functions
                  #'gfm-tables--cell-edit-after-commit
                  'append 'local))
      (with-current-buffer buf (edit-indirect-commit))
      (with-current-buffer src-buf
        (goto-char (point-min))
        (forward-line 2)
        (let ((after-line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
          (should (equal before-line after-line)))))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-strips-newlines ()
  (with-temp-buffer
    (insert "a\nb\nc")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a b c" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-escapes-pipe ()
  (with-temp-buffer
    (insert "a|b")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-keeps-existing-escape ()
  (with-temp-buffer
    (insert "a\\|b")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-leading-pipe ()
  (with-temp-buffer
    (insert "|a")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "\\|a" (buffer-string)))))

;;; Cell bounds + active-cell highlight

(ert-deftest lang-markdown/gfm-tables-cell-bounds-simple ()
  (with-temp-buffer
    (insert "| A | B |")
    (let ((cb (gfm-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb)))
      ;; First cell content begins right after the leading `|'.
      (should (eq ?| (char-before (car (nth 0 cb)))))
      (should (eq ?| (char-after  (cdr (nth 0 cb))))))))

(ert-deftest lang-markdown/gfm-tables-cell-bounds-honours-escape ()
  (with-temp-buffer
    (insert "| a \\| b | c |")
    (let ((cb (gfm-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb))))))

(ert-deftest lang-markdown/gfm-tables-cell-info-at-point ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (let ((info (gfm-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info))))))

(ert-deftest lang-markdown/gfm-tables-active-cell-highlight-applied ()
  "Display string carries the active-cell face after entering the row."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (gfm-tables--update-cursor-highlight)
    (let* ((ov gfm-tables--highlighted-row-ov)
           (disp (overlay-get ov 'display))
           (has-face (cl-some
                      (lambda (i)
                        (let ((f (get-text-property i 'face disp)))
                          (or (eq f 'gfm-tables-active-cell-face)
                              (and (listp f)
                                   (memq 'gfm-tables-active-cell-face f)))))
                      (number-sequence 0 (1- (length disp))))))
      (should has-face)
      (should gfm-tables--cursor-anchor))))

(ert-deftest lang-markdown/gfm-tables-cursor-highlight-restores-off-row ()
  "Moving point out of a table restores the cursor and original display."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\nout of table\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (gfm-tables--update-cursor-highlight)
    (let ((row-ov gfm-tables--highlighted-row-ov))
      (goto-char (point-max))
      (gfm-tables--update-cursor-highlight)
      (should-not gfm-tables--highlighted-row-ov)
      (should-not (overlay-get row-ov 'gfm-tables-saved-display))
      (should-not gfm-tables--cursor-anchor))))

;;; Header column reordering

(ert-deftest lang-markdown/gfm-tables-swap-column-right-swaps ()
  (with-temp-buffer
    (insert "| A | B | C |\n| - | - | - |\n| 1 | 2 | 3 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (gfm-tables-swap-column-right)
    (goto-char (point-min))
    (let ((header-line (buffer-substring-no-properties
                        (point) (line-end-position)))
          (body-line (progn (forward-line 2)
                            (buffer-substring-no-properties
                             (point) (line-end-position)))))
      (should (string-match-p "B.*A" header-line))
      (should (string-match-p "2.*1" body-line)))))

(ert-deftest lang-markdown/gfm-tables-swap-column-left-edge-noop ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (let ((before (buffer-string)))
      (gfm-tables-swap-column-left)
      (should (equal (buffer-string) before)))))

(ert-deftest lang-markdown/gfm-tables-swap-column-on-body-noop ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (let ((before (buffer-string)))
      (gfm-tables-swap-column-right)
      (should (equal (buffer-string) before)))))

;;; Cell-wise navigation

(ert-deftest lang-markdown/gfm-tables-cell-forward-moves-cell ()
  (with-temp-buffer
    (insert "| A | B | C |\n| - | - | - |\n| 1 | 2 | 3 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point))) ; stand on the digit
    (let ((before (gfm-tables--cell-info-at-point)))
      (should before)
      (gfm-tables-cell-forward)
      (let ((after (gfm-tables--cell-info-at-point)))
        (should after)
        (should (= (1+ (cdr before)) (cdr after)))))))

(ert-deftest lang-markdown/gfm-tables-cell-tab-wraps-to-next-row ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "B")
    (goto-char (1- (point)))
    (gfm-tables-cell-tab)
    (let ((info (gfm-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info)))
      ;; Landed on body row 1 (`1' digit nearby).
      (should (string-match-p "1" (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))

(ert-deftest lang-markdown/gfm-tables-cell-tab-inserts-row-at-end ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "2")
    (goto-char (1- (point)))
    (gfm-tables-cell-tab)
    (let ((info (gfm-tables--cell-info-at-point)))
      (should info)
      (should (= 0 (cdr info)))
      ;; New body row is empty: source line of the form `|  |  |'.
      (should (string-match-p "^|[[:space:]]*|[[:space:]]*|"
                              (buffer-substring (line-beginning-position)
                                                (line-end-position)))))))

(ert-deftest lang-markdown/gfm-tables-cell-backtab-wraps-to-prev-row ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (gfm-tables-cell-backtab)
    (let ((info (gfm-tables--cell-info-at-point)))
      (should info)
      ;; Wrapped to header row's last cell.
      (should (= 1 (cdr info)))
      (should (string-match-p "B" (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))))

(ert-deftest lang-markdown/gfm-tables-row-down-skips-delim ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "A")
    (goto-char (1- (point)))
    (gfm-tables-row-down)
    ;; The body row's source line begins with `|', followed by ` 1 ' for cell 0.
    ;; row-down lands on the first content char, which is the space right after `|'.
    (should (string-match-p "^| 1 "
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))
    (should (eq ?\s (char-after (point))))))

(ert-deftest lang-markdown/gfm-tables-row-down-stops-at-table-edge ()
  "Moving down from the last row of a table does not jump into the next table."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "Some prose here.\n\n"
            "| C | D |\n| - | - |\n| 9 | 8 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "1")
    (goto-char (1- (point)))
    (should (gfm-tables--cell-info-at-point))
    (should-not (gfm-tables--row-on-relative-line 1))))

(ert-deftest lang-markdown/gfm-tables-row-up-stops-at-table-edge ()
  "Moving up from the first body row of a table stays at the header row."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "Prose.\n\n"
            "| C | D |\n| - | - |\n| 9 | 8 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "9")
    (goto-char (1- (point)))
    (should (gfm-tables--cell-info-at-point))
    (let ((up (gfm-tables--row-on-relative-line -1)))
      ;; The only row above 9 inside this block is its header row.
      (should up)
      (let ((header-line
             (save-excursion (goto-char (overlay-start up))
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))))
        (should (string-match-p "C" header-line))))))

(ert-deftest lang-markdown/gfm-tables-snap-from-non-cell-point ()
  "Snap moves a non-cell point on a row line into the first cell."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position)) ; on `|', not in any cell
    (let ((before (point)))
      (gfm-tables--maybe-snap-to-cell)
      (should (> (point) before))
      (let* ((info (gfm-tables--cell-info-at-point))
             (cb (overlay-get (car info) 'gfm-tables-cell-bounds))
             (cell0 (nth 0 cb)))
        (should (>= (point) (car cell0)))
        (should (< (point) (cdr cell0)))))))

(ert-deftest lang-markdown/gfm-tables-snap-noop-when-in-cell ()
  "Snap leaves point alone when already inside a cell range."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "2")
    (goto-char (1- (point)))
    (let ((before (point)))
      (gfm-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-tables-snap-skips-invisible-row ()
  "Snap is a no-op when the row line is invisible."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (let ((lbeg (line-beginning-position))
          (lend (line-end-position)))
      (put-text-property lbeg lend 'invisible t)
      (add-to-invisibility-spec t)
      (goto-char lbeg)
      (let ((before (point)))
        (gfm-tables--maybe-snap-to-cell)
        (should (= before (point)))))))

(ert-deftest lang-markdown/gfm-tables-snap-skipped-during-isearch ()
  "Snap is a no-op while `isearch-mode' is active so isearch can park
point on a column gap or border without being yanked back to cell 0."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position)) ; on `|', not in any cell
    (let ((before (point))
          (isearch-mode " Isearch"))
      (gfm-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-tables-snap-skipped-for-search-commands ()
  "Snap is a no-op when `this-command' is a search-style command, so
evil-search-next and friends don't have point yanked back."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (goto-char (point-min))
    (search-forward "| 1 ")
    (goto-char (line-beginning-position))
    (let ((before (point))
          (this-command 'evil-search-next))
      (gfm-tables--maybe-snap-to-cell)
      (should (= before (point))))))

(ert-deftest lang-markdown/gfm-tables-isearch-advances-when-match-ends-on-pipe ()
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
    (gfm-tables-mode 1)
    (let ((gfm-tables--rebuild-timer nil)) (gfm-tables--rebuild))
    (goto-char (point-min))
    ;; Simulate the body of isearch's command loop: search, then run
    ;; post-command-hook with `isearch-mode' active.  Match-end of
    ;; "foo " lands on a `|' (outside cell bounds) — the snap, if not
    ;; suppressed, would rewind point and trap the next search.
    (let ((isearch-mode " Isearch")
          points)
      (should (re-search-forward "foo " nil t))
      (gfm-tables--update-cursor-highlight)
      (push (point) points)
      (should (re-search-forward "foo " nil t))
      (gfm-tables--update-cursor-highlight)
      (push (point) points)
      (setq points (nreverse points))
      (should (< (nth 0 points) (nth 1 points))))))

;;; Evil shim

(ert-deftest lang-markdown/gfm-tables-evil-edit-commands-listed ()
  (should (boundp 'gfm-tables--evil-edit-commands))
  (should (memq 'evil-insert gfm-tables--evil-edit-commands))
  (should (memq 'evil-change gfm-tables--evil-edit-commands))
  (should (memq 'evil-open-below gfm-tables--evil-edit-commands)))

;;; Mode lifecycle

(ert-deftest lang-markdown/gfm-tables-mode-creates-overlays ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-tables))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-mode-removes-overlays ()
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (gfm-tables-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-tables))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-enabled-via-gfm-mode-hook ()
  (should (memq 'gfm-tables-mode gfm-mode-hook)))

;;; Narrowing-resilient discovery and teardown — tables

(defun lang-markdown-tests--two-slide-tables-buffer ()
  "Insert a two-slide buffer with one table per slide; return widened mark."
  (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
  (insert "\n# slide 2\n\n")
  (insert "| C | D |\n| - | - |\n| 3 | 4 |\n"))

(ert-deftest lang-markdown/gfm-tables-narrowed-rebuild-does-not-signal ()
  "Narrowed rebuild over a widened cache must not raise `args-out-of-range'.
Pre-fix, `gfm-tables--apply-table' touched header positions outside the
narrowing and signalled.  See fix-gfm-narrowing-safety."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-tables-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (should (progn (gfm-tables--rebuild) t)))))

(ert-deftest lang-markdown/gfm-tables-narrowed-rebuild-no-zombies ()
  "Post-`widen' the tracking list length matches the on-buffer overlay count.
Pre-fix the full-clear `--remove-overlays' under narrowing left
off-narrowing overlays untracked."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (lang-markdown-tests--two-slide-tables-buffer)
    (gfm-tables-mode 1)
    (let ((slide-1-end (save-excursion
                         (goto-char (point-min))
                         (search-forward "# slide 2")
                         (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-tables--rebuild)
      (widen)
      (let ((on-buffer (cl-count-if
                        (lambda (ov) (overlay-get ov 'gfm-tables))
                        (overlays-in (point-min) (point-max)))))
        (should (= (length gfm-tables--overlays) on-buffer))))))

;;; Per-rebuild width cache

(ert-deftest lang-markdown/gfm-tables-width-cache-fast-path-matches-uncached ()
  "Fast path returns same width as full computation for plain strings."
  (let ((s "hello world"))
    (let ((gfm-tables--width-cache nil))
      (should (= (string-width s) (gfm-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-tables-width-cache-honours-display-prop ()
  "Cached width matches uncached for cells with `display' replacements."
  (let* ((s (concat "abc" (propertize "X" 'display "yyy") "de"))
         (uncached (let ((gfm-tables--width-cache nil))
                     (gfm-tables--visible-width s)))
         (cache (make-hash-table :test 'eq)))
    (let ((gfm-tables--width-cache cache))
      (should (= uncached (gfm-tables--visible-width s)))
      ;; Second call hits the cache.
      (should (= uncached (gfm-tables--visible-width s)))
      (should (gethash s cache)))))

(ert-deftest lang-markdown/gfm-tables-visible-width-ignores-auto-compositions ()
  "Auto-compositions (e.g. ligatures) must not shrink visible-width.
Overlay display strings do not run `composition-function-table', so
counting an auto-composition would under-pad the cell."
  (let ((s (concat (propertize "x" 'invisible 'gfm-test)
                   "fl"))
        (buffer-invisibility-spec '((gfm-test . t))))
    ;; Even if `find-composition' would report a composition for "fl"
    ;; in the current buffer, the cell string carries no `composition'
    ;; text-property, so width must equal the underlying char widths.
    (should (= 2 (gfm-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-tables-width-cache-honours-composition-prop ()
  "Cached width matches uncached for cells with `composition' property."
  (let ((s (copy-sequence "abcXXXXXde")))
    (compose-string s 3 8 ?Y)
    (let* ((uncached (let ((gfm-tables--width-cache nil))
                       (gfm-tables--visible-width s)))
           (cache (make-hash-table :test 'eq))
           (gfm-tables--width-cache cache))
      (should (= uncached (gfm-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-tables-width-cache-honours-invisible-prop ()
  "Cached width matches uncached for cells with hidden `invisible' segments."
  (let* ((s (concat "abc" (propertize "HIDE" 'invisible 'gfm-test) "de"))
         (buffer-invisibility-spec '((gfm-test . t)))
         (uncached (let ((gfm-tables--width-cache nil))
                     (gfm-tables--visible-width s)))
         (cache (make-hash-table :test 'eq))
         (gfm-tables--width-cache cache))
    (should (= 5 uncached))
    (should (= uncached (gfm-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-tables-width-cache-not-shared-across-eq-distinct-strings ()
  "Cache is `eq'-keyed; two equal-but-distinct strings do not share entries."
  (let* ((cache (make-hash-table :test 'eq))
         (gfm-tables--width-cache cache)
         (a (copy-sequence "hello"))
         (b (copy-sequence "hello")))
    (gfm-tables--visible-width a)
    (should (gethash a cache))
    (should-not (gethash b cache))))

(ert-deftest lang-markdown/gfm-tables-width-cache-substring-not-stale ()
  "A substring of a fontified cell is measured fresh, not from the parent's hit."
  (let* ((parent (gfm-tables--fontify-cell "hello world"))
         (cache (make-hash-table :test 'eq))
         (gfm-tables--width-cache cache)
         (parent-w (gfm-tables--visible-width parent))
         (sub (substring parent 0 5))
         (sub-w (gfm-tables--visible-width sub)))
    (should (= parent-w (gfm-tables--visible-width parent)))
    (should (= sub-w (gfm-tables--visible-width sub)))
    (should (= 5 sub-w))
    (should-not (= parent-w sub-w))))

;;; Shared row layout

(ert-deftest lang-markdown/gfm-tables-row-layout-feeds-both-helpers ()
  "Layout-based helpers produce strings/bounds equal to the legacy callers.
Verifies the packed bounds vector matches the legacy nested-list shape
emitted by `gfm-tables--multiline-row-char-bounds'."
  (dolist (role '(header body-default body-alt))
    (let* ((cells '("alpha" "beta gamma" "d"))
           (col-widths (vector 5 4 1))
           (layout (gfm-tables--row-layout cells col-widths))
           (composed-direct (gfm-tables--compose-multiline-row
                             cells col-widths role))
           (composed-via-layout (gfm-tables--compose-row-from-layout
                                 layout col-widths role))
           (bounds-direct (gfm-tables--multiline-row-char-bounds
                           cells col-widths))
           (vec (gfm-tables--row-layout-bounds-vec layout))
           (n-cells (gfm-tables--row-layout-n-cells layout)))
      (should (equal composed-direct composed-via-layout))
      (cl-loop for line-cb in bounds-direct
               for line from 0
               do (cl-loop for cb in line-cb
                           for cell from 0
                           for base = (* 2 (+ (* line n-cells) cell))
                           do (should (= (car cb) (aref vec base)))
                              (should (= (cdr cb) (aref vec (1+ base)))))))))

;;; Scoped rebuild

(defun gfm-tables--test-overlay-set ()
  "Return the gfm-tables overlay objects in the current buffer as a hash set."
  (let ((set (make-hash-table :test 'eq)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-tables)
        (puthash ov t set)))
    set))

(defun gfm-tables--test-block-overlays (h-beg b-end)
  "Return gfm-tables overlays whose start lies in H-BEG..B-END."
  (cl-remove-if-not
   (lambda (ov)
     (and (overlay-get ov 'gfm-tables)
          (let ((s (overlay-start ov)))
            (and s (<= h-beg s) (<= s b-end)))))
   (overlays-in h-beg (1+ b-end))))

(ert-deftest lang-markdown/gfm-tables-scoped-edit-inside-single-table ()
  "Editing inside one table only rebuilds that table's overlays."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "| C | D |\n| - | - |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (let* ((blocks (gfm-tables--find-blocks))
           (b1 (nth 0 blocks))
           (b2 (nth 1 blocks))
           (b1-h (nth 0 b1)) (b1-e (nth 3 b1))
           (b2-h (nth 0 b2)) (b2-e (nth 3 b2))
           (b1-before (gfm-tables--test-block-overlays b1-h b1-e))
           (b2-before (gfm-tables--test-block-overlays b2-h b2-e)))
      (setq gfm-tables--dirty-region
            (cons (1+ b1-h) (1- b1-e)))
      (gfm-tables--rebuild-scoped)
      (let ((b1-after (gfm-tables--test-block-overlays b1-h b1-e))
            (b2-after (gfm-tables--test-block-overlays b2-h b2-e)))
        (should-not (cl-intersection b1-before b1-after))
        (should (cl-every (lambda (o) (memq o b2-after)) b2-before))
        (should (cl-every (lambda (o) (memq o b2-before)) b2-after))))))

(ert-deftest lang-markdown/gfm-tables-scoped-edit-outside-tables-noop ()
  "Editing in a region intersecting no decorated table is a no-op."
  (with-temp-buffer
    (insert "intro line\n\n| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (let ((before (gfm-tables--test-overlay-set))
          (rebuild-count (alist-get 'rebuild-count gfm-tables--stats)))
      (setq gfm-tables--dirty-region (cons 1 5))
      (gfm-tables--rebuild-scoped)
      (let ((after (gfm-tables--test-overlay-set)))
        (should (= (hash-table-count before) (hash-table-count after)))
        (maphash (lambda (ov _) (should (gethash ov after))) before)
        (should (= rebuild-count
                   (alist-get 'rebuild-count gfm-tables--stats)))))))

(ert-deftest lang-markdown/gfm-tables-scoped-edit-spans-two-tables-full-rebuild ()
  "Edit region intersecting two tables triggers a full rebuild."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n\n"
            "| C | D |\n| - | - |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (let* ((blocks (gfm-tables--find-blocks))
           (b1-h (nth 0 (car blocks)))
           (b2-e (nth 3 (cadr blocks)))
           (before (gfm-tables--test-overlay-set)))
      (setq gfm-tables--dirty-region (cons b1-h b2-e))
      (gfm-tables--rebuild-scoped)
      (let ((after (gfm-tables--test-overlay-set)))
        (should (cl-every (lambda (ov) (not (gethash ov after)))
                          (let (xs) (maphash (lambda (k _) (push k xs)) before)
                               xs)))))))

(ert-deftest lang-markdown/gfm-tables-scoped-edit-overlapping-fence-full-rebuild ()
  "Edit region overlapping a code-fence line triggers a full rebuild."
  (with-temp-buffer
    (gfm-mode)
    (insert "```\nfenced\n```\n\n| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (let* ((fence (car (gfm-code-fences--find-blocks)))
           (open-line-beg (save-excursion
                            (goto-char (nth 0 fence)) (line-beginning-position)))
           (open-line-end (save-excursion
                            (goto-char (nth 0 fence)) (line-end-position)))
           (before (gfm-tables--test-overlay-set)))
      (setq gfm-tables--dirty-region (cons open-line-beg open-line-end))
      (gfm-tables--rebuild-scoped)
      (let ((after (gfm-tables--test-overlay-set)))
        ;; Full rebuild → overlay objects are fresh.
        (should (cl-every (lambda (ov) (not (gethash ov after)))
                          (let (xs) (maphash (lambda (k _) (push k xs)) before)
                               xs)))))))

(ert-deftest lang-markdown/gfm-tables-per-window-display-overlays ()
  "Each window showing the buffer gets its own display-overlay set.
Anchor overlays stay shared across windows; only display overlays
carry a `window' restriction."
  (let ((buf (generate-new-buffer "*gfm-tables-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other buf)
            (with-current-buffer buf
              (gfm-tables-mode 1)
              (let* ((overlays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-tables))
                                (overlays-in (point-min) (point-max))))
                     (anchors (cl-count-if
                               (lambda (o) (overlay-get o 'gfm-tables-anchor))
                               overlays))
                     (displays (cl-remove-if-not
                                (lambda (o) (overlay-get o 'gfm-tables-display))
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

(ert-deftest lang-markdown/gfm-tables-reconcile-windows-touches-changed-only ()
  "Reconciling windows replaces only the resized window's display overlays.
Untouched windows keep their existing display-overlay objects (eq)."
  (let ((buf (generate-new-buffer "*gfm-tables-reconcile-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-tables-mode 1)
              (let* ((displays-for (lambda (w)
                                     (cl-remove-if-not
                                      (lambda (o)
                                        (and (overlay-get o 'gfm-tables-display)
                                             (eq (overlay-get o 'window) w)))
                                      gfm-tables--overlays)))
                     (a-before (funcall displays-for win-a))
                     (b-before (funcall displays-for win-b)))
                ;; Forge a width change for win-a only by mutating the cached
                ;; state; reconcile sees win-a as resized, win-b as unchanged.
                (setq gfm-tables--last-window-state
                      (mapcar (lambda (e)
                                (if (eq (car e) win-a)
                                    (cons (car e) (1- (cdr e)))
                                  e))
                              gfm-tables--last-window-state))
                (gfm-tables--reconcile-windows)
                (let ((a-after (funcall displays-for win-a))
                      (b-after (funcall displays-for win-b)))
                  ;; win-a's overlays were replaced with fresh ones.
                  (should-not (cl-intersection a-before a-after))
                  ;; win-b's overlays are untouched (same objects, same count).
                  (should (= (length b-before) (length b-after)))
                  (should (cl-every (lambda (o) (memq o b-after)) b-before)))))
            (delete-window win-b)))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-tables-highlight-targets-selected-window ()
  "Active-cell highlight paints the selected window's display overlay only.
Per-window display overlays let two windows render the same buffer at
different widths; the cell-edit highlight should only affect the
window holding point."
  (let ((buf (generate-new-buffer "*gfm-tables-hl-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "| A | B |\n| - | - |\n| 1 | 2 |\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-tables-mode 1)
              (goto-char (point-min))
              (forward-line 2)
              (forward-char 2)
              (with-selected-window win-a
                (gfm-tables--update-cursor-highlight))
              (let* ((info (gfm-tables--cell-info-at-point))
                     (anchor (car info))
                     (a-display (gfm-tables--display-overlay-for-anchor anchor win-a))
                     (b-display (gfm-tables--display-overlay-for-anchor anchor win-b)))
                ;; Selected window's overlay carries the saved-display sentinel.
                (should (overlay-get a-display 'gfm-tables-saved-display))
                ;; The other window's overlay does not.
                (should-not (overlay-get b-display 'gfm-tables-saved-display))))
            (delete-window win-b)))
      (kill-buffer buf))))

(ert-deftest lang-markdown/gfm-tables-window-config-change-wired-to-full-rebuild ()
  "`window-configuration-change-hook' is wired to the full-rebuild scheduler."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (should (memq 'gfm-tables--schedule-full-rebuild
                  window-configuration-change-hook))
    (should-not (memq 'gfm-tables--schedule-rebuild
                      window-configuration-change-hook))))

(ert-deftest lang-markdown/gfm-tables-block-visible-p ()
  "`gfm-tables--block-visible-p' detects overlap with any window range."
  (let ((block '(100 110 120 200)))
    ;; Block fully inside a single range.
    (should (gfm-tables--block-visible-p block '((50 . 250))))
    ;; Range fully inside block.
    (should (gfm-tables--block-visible-p block '((130 . 180))))
    ;; Edge-touching counts as visible.
    (should (gfm-tables--block-visible-p block '((50 . 100))))
    (should (gfm-tables--block-visible-p block '((200 . 250))))
    ;; No range overlaps.
    (should-not (gfm-tables--block-visible-p block '((1 . 99) (201 . 300))))
    ;; Empty list of ranges.
    (should-not (gfm-tables--block-visible-p block nil))
    ;; Multiple ranges; one covers it.
    (should (gfm-tables--block-visible-p block '((1 . 50) (130 . 180))))))

(ert-deftest lang-markdown/gfm-tables-schedule-full-rebuild-noop-when-window-state-unchanged ()
  "Full-rebuild scheduler is a no-op when the window state is unchanged."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    ;; Cancel any timer the mode-enable rebuild may have left running.
    (when (timerp gfm-tables--rebuild-timer)
      (cancel-timer gfm-tables--rebuild-timer))
    (setq gfm-tables--rebuild-timer nil)
    ;; Same window state → no timer armed.
    (gfm-tables--schedule-full-rebuild)
    (should-not gfm-tables--rebuild-timer)
    ;; Forge a state change → timer armed.
    (setq gfm-tables--last-window-state (cons 'forged gfm-tables--last-window-state))
    (gfm-tables--schedule-full-rebuild)
    (should (timerp gfm-tables--rebuild-timer))
    (cancel-timer gfm-tables--rebuild-timer)))

;;; Phase-level instrumentation

(ert-deftest lang-markdown/gfm-tables-phase-totals-non-negative-and-bounded ()
  "Phase totals are non-negative and sum to ≤ recorded total-time."
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (gfm-tables-mode 1)
    (let* ((stats gfm-tables--stats)
           (total (alist-get 'total-time stats))
           (phases (alist-get 'phase-totals stats)))
      (should phases)
      (dolist (p phases)
        (should (>= (cdr p) 0)))
      (let ((sum (cl-loop for p in phases sum (cdr p))))
        ;; Allow a small fudge for clock noise between outer total and phases.
        (should (<= sum (+ total 0.001)))))))

(ert-deftest lang-markdown/gfm-tables-phase-totals-include-required-keys ()
  "Phase totals include all five keys required by the spec."
  (with-temp-buffer
    (insert "| A |\n| - |\n| 1 |\n")
    (gfm-tables-mode 1)
    (let ((phases (alist-get 'phase-totals gfm-tables--stats)))
      (dolist (k '(find-blocks parse layout compose apply))
        (should (assq k phases))))))

;;; Reveal

;;; gfm-links tests

(require '+gfm-links)

(defmacro lang-markdown-tests--with-links-buffer (contents &rest body)
  "Run BODY in a `markdown-mode' temp buffer holding CONTENTS, links mode on."
  (declare (indent 1))
  `(with-temp-buffer
     (delay-mode-hooks (markdown-mode))
     (setq-local markdown-hide-urls t)
     (insert ,contents)
     (gfm-links-mode 1)
     ,@body))

(defun lang-markdown-tests--link-overlays ()
  "Return gfm-links overlays in the current buffer, sorted by start."
  (sort (copy-sequence gfm-links--overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun lang-markdown-tests--link-overlay-at (pos &optional side)
  "Return a gfm-links overlay covering POS, optionally matching SIDE."
  (cl-find-if (lambda (o)
                (and (overlay-get o 'gfm-links-revealable)
                     (<= (overlay-start o) pos)
                     (< pos (overlay-end o))
                     (or (null side) (eq side (overlay-get o 'gfm-links-side)))))
              gfm-links--overlays))

;;; Mode toggle

(ert-deftest lang-markdown/gfm-links-mode-creates-overlays ()
  "Enabling the mode decorates a recognised link."
  (lang-markdown-tests--with-links-buffer
      "See [Anthropic](https://anthropic.com) here.\n"
    (should (lang-markdown-tests--link-overlay-at 6 'title))
    (should (lang-markdown-tests--link-overlay-at 18 'url))))

(ert-deftest lang-markdown/gfm-links-mode-removes-overlays ()
  "Disabling the mode removes every overlay it created."
  (lang-markdown-tests--with-links-buffer
      "See [Anthropic](https://anthropic.com) here.\n"
    (should gfm-links--overlays)
    (gfm-links-mode -1)
    (should-not gfm-links--overlays)
    (should-not (cl-some (lambda (o) (overlay-get o 'gfm-links))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-links-hide-urls-off-disables-mode ()
  "Setting `markdown-hide-urls' to nil disables the mode and clears overlays."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "See [Anthropic](https://anthropic.com).\n")
    (gfm-links--maybe-enable)
    (should gfm-links-mode)
    (setq-local markdown-hide-urls nil)
    (should-not gfm-links-mode)
    (should-not gfm-links--overlays)))

(ert-deftest lang-markdown/gfm-links-enabled-via-gfm-mode-hook ()
  "The maybe-enable hook is wired into `gfm-mode-hook'."
  (should (memq 'gfm-links--maybe-enable gfm-mode-hook)))

;;; 14.1 Per-shape decoration

(ert-deftest lang-markdown/gfm-links-inline-with-title-attr ()
  "Inline link with a title attribute is decorated and exposes the attr."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com \"official\")\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (eq 'inline (overlay-get ov 'gfm-links-kind)))
      (should (equal "https://anthropic.com" (overlay-get ov 'gfm-links-url)))
      (should (equal "official" (overlay-get ov 'gfm-links-title-attr)))
      (should (equal "Anthropic"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-links-reference-full ()
  "Full reference link resolves through the definition alist."
  (lang-markdown-tests--with-links-buffer
      "[docs][d] here.\n\n[d]: https://example.com\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (eq 'reference (overlay-get ov 'gfm-links-kind)))
      (should (equal "https://example.com" (overlay-get ov 'gfm-links-url))))))

(ert-deftest lang-markdown/gfm-links-reference-collapsed ()
  "Collapsed reference link `[text][]' resolves using the text as label."
  (lang-markdown-tests--with-links-buffer
      "[design][] here.\n\n[design]: ./docs/adr-001.md\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 2 'title)))
      (should ov)
      (should (equal "./docs/adr-001.md" (overlay-get ov 'gfm-links-url))))))

(ert-deftest lang-markdown/gfm-links-reference-shortcut ()
  "Shortcut reference `[label]' is decorated only when a definition exists."
  (lang-markdown-tests--with-links-buffer
      "Prose [design] more.\n\n[design]: ./adr.md\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'reference (overlay-get ov 'gfm-links-kind)))
      (should (equal "./adr.md" (overlay-get ov 'gfm-links-url))))))

(ert-deftest lang-markdown/gfm-links-reference-shortcut-undefined-not-decorated ()
  "Shortcut `[label]' with no matching definition is left raw."
  (lang-markdown-tests--with-links-buffer
      "Prose [design] more, no definition.\n"
    (should-not (lang-markdown-tests--link-overlay-at 8))))

(ert-deftest lang-markdown/gfm-links-autolink-host-label ()
  "An autolink is decorated with the host as the visible label."
  (lang-markdown-tests--with-links-buffer
      "Visit <https://anthropic.com/path> now.\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'autolink (overlay-get ov 'gfm-links-kind)))
      (should (equal "anthropic.com"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-links-bare-url ()
  "A GFM bare URL is decorated."
  (lang-markdown-tests--with-links-buffer
      "Visit https://anthropic.com today.\n"
    (let ((ov (lang-markdown-tests--link-overlay-at 8 'title)))
      (should ov)
      (should (eq 'bare-url (overlay-get ov 'gfm-links-kind)))
      (should (equal "anthropic.com"
                     (substring-no-properties (overlay-get ov 'display)))))))

(ert-deftest lang-markdown/gfm-links-wiki-link ()
  "A wiki link is decorated when `markdown-enable-wiki-links' is on."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (setq-local markdown-enable-wiki-links t)
    (insert "See [[Some Page]] here.\n")
    (gfm-links-mode 1)
    (let ((ov (lang-markdown-tests--link-overlay-at 6 'title)))
      (should ov)
      (should (eq 'wiki (overlay-get ov 'gfm-links-kind))))))

(ert-deftest lang-markdown/gfm-links-image-not-decorated ()
  "Image links are explicitly left raw."
  (lang-markdown-tests--with-links-buffer
      "![alt](./diagram.png)\n"
    (should-not gfm-links--overlays)))

(ert-deftest lang-markdown/gfm-links-reference-definition-not-decorated ()
  "Reference-definition lines themselves are not decorated."
  (lang-markdown-tests--with-links-buffer
      "[d]: https://example.com\n"
    (should-not gfm-links--overlays)))

;;; 14.2 Reference resolution

(ert-deftest lang-markdown/gfm-links-ref-def-alist-build ()
  "The ref-def alist maps a downcased label to its URL, title, and position."
  (lang-markdown-tests--with-links-buffer
      "[D]: https://example.com \"a title\"\n"
    (let ((entry (gfm-links--resolve-ref "d")))
      (should entry)
      (should (equal "https://example.com" (nth 0 entry)))
      (should (equal "a title" (nth 1 entry)))
      (should (integerp (nth 2 entry))))))

(ert-deftest lang-markdown/gfm-links-ref-def-first-wins ()
  "When a label is defined twice, the first definition wins."
  (lang-markdown-tests--with-links-buffer
      "[d]: https://first.example\n[d]: https://second.example\n"
    (should (equal "https://first.example" (nth 0 (gfm-links--resolve-ref "d"))))))

(ert-deftest lang-markdown/gfm-links-broken-reference-not-decorated ()
  "A reference link whose label has no definition is not decorated."
  (lang-markdown-tests--with-links-buffer
      "[title][missing] here.\n"
    (should-not (lang-markdown-tests--link-overlay-at 2))))

(ert-deftest lang-markdown/gfm-links-ref-def-recomputed-on-rebuild ()
  "Editing a definition line and rebuilding re-resolves reference links."
  (lang-markdown-tests--with-links-buffer
      "[docs][d]\n\n[d]: https://old.example\n"
    (should (equal "https://old.example"
                   (overlay-get (lang-markdown-tests--link-overlay-at 2 'title)
                                'gfm-links-url)))
    (goto-char (point-min))
    (search-forward "https://old.example")
    (replace-match "https://new.example")
    (gfm-links--rebuild)
    (should (equal "https://new.example"
                   (overlay-get (lang-markdown-tests--link-overlay-at 2 'title)
                                'gfm-links-url)))))

;;; 5.x Icon resolution

(ert-deftest lang-markdown/gfm-links-icon-for-http-url ()
  "An http(s) URL resolves through `nerd-icons-icon-for-url'."
  (skip-unless (fboundp 'nerd-icons-icon-for-url))
  (should (equal (nerd-icons-icon-for-url "https://github.com/foo/bar")
                 (gfm-links--icon-for-target "https://github.com/foo/bar"))))

(ert-deftest lang-markdown/gfm-links-icon-for-relative-file ()
  "A relative path resolves through `nerd-icons-icon-for-file' on the basename."
  (skip-unless (fboundp 'nerd-icons-icon-for-file))
  (should (equal (nerd-icons-icon-for-file "init.el")
                 (gfm-links--icon-for-target "./modules/init.el"))))

(ert-deftest lang-markdown/gfm-links-label-for-naked-url ()
  "The naked-URL label is the URL host."
  (should (equal "anthropic.com"
                 (gfm-links--label-for-naked-url
                  "https://anthropic.com/some/path"))))

;;; 7.x Suppression of the built-in compose path

(ert-deftest lang-markdown/gfm-links-suppresses-composition ()
  "With the mode on, the URL region gets no `composition' text property."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "(")
    (should-not (get-text-property (point) 'composition))))

(ert-deftest lang-markdown/gfm-links-composition-applies-when-mode-off ()
  "With the mode off but `markdown-hide-urls' on, composition still applies."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "[Anthropic](https://anthropic.com)\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "(")
    (should (get-text-property (point) 'composition))))

;;; 8.x Cursor reveal

(ert-deftest lang-markdown/gfm-links-reveal-whole-link ()
  "Point on the title reveals both the title-side and url-side overlays."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (let ((title-ov (lang-markdown-tests--link-overlay-at 2 'title))
          (url-ov (lang-markdown-tests--link-overlay-at 13 'url)))
      (should (overlay-get title-ov 'display))
      (should (overlay-get url-ov 'display))
      (goto-char 3)
      (gfm-links--reveal)
      (should-not (overlay-get title-ov 'display))
      (should-not (overlay-get url-ov 'display))
      (goto-char (point-max))
      (gfm-links--reveal)
      (should (overlay-get title-ov 'display))
      (should (overlay-get url-ov 'display)))))

(ert-deftest lang-markdown/gfm-links-reveal-respects-window-restriction ()
  "Reveal in window A does not expose the link via window B's overlays."
  (let ((buf (generate-new-buffer "*gfm-links-reveal-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (delay-mode-hooks (markdown-mode))
            (setq-local markdown-hide-urls t)
            (insert "[Anthropic](https://anthropic.com)\n"))
          (set-window-buffer (selected-window) buf)
          (let* ((win-a (selected-window))
                 (win-b (split-window)))
            (set-window-buffer win-b buf)
            (with-current-buffer buf
              (gfm-links-mode 1)
              (with-selected-window win-a
                (goto-char 3)
                (gfm-links--reveal))
              (let* ((title-ovs
                      (cl-remove-if-not
                       (lambda (o) (eq 'title (overlay-get o 'gfm-links-side)))
                       gfm-links--overlays))
                     (a-ov (cl-find-if
                            (lambda (o) (eq (overlay-get o 'window) win-a))
                            title-ovs))
                     (b-ov (cl-find-if
                            (lambda (o) (eq (overlay-get o 'window) win-b))
                            title-ovs)))
                (should a-ov)
                (should b-ov)
                (should-not (overlay-get a-ov 'display))
                (should (overlay-get b-ov 'display))))
            (delete-window win-b)))
      (kill-buffer buf))))

;;; 9.x RET / follow-link

(ert-deftest lang-markdown/gfm-links-ret-bound-on-title-overlay ()
  "`RET' resolves to the follow command inside a decorated link only."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain prose.\n"
    (goto-char 3)
    (should (eq 'gfm-links-follow-link-at-point (key-binding (kbd "RET"))))
    (goto-char (point-max))
    (should-not (eq 'gfm-links-follow-link-at-point (key-binding (kbd "RET"))))))

(ert-deftest lang-markdown/gfm-links-follow-link-browses-url ()
  "`gfm-links-follow-link-at-point' browses the resolved URL on a link."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com)\n"
    (let (browsed)
      (cl-letf (((symbol-function 'markdown--browse-url)
                 (lambda (url) (setq browsed url))))
        (goto-char 3)
        (gfm-links-follow-link-at-point)
        (should (equal "https://anthropic.com" browsed))))))

(ert-deftest lang-markdown/gfm-links-follow-link-off-link-errors ()
  "`gfm-links-follow-link-at-point' off any decorated link signals a user error."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain.\n"
    (goto-char (point-max))
    (should-error (gfm-links-follow-link-at-point) :type 'user-error)))

;;; 10.x xref backend

(ert-deftest lang-markdown/gfm-links-xref-backend-claims-reference-link ()
  "The xref backend claims a reference link and defers off one."
  (lang-markdown-tests--with-links-buffer
      "[docs][d] then [inline](https://x.example)\n\n[d]: https://d.example\n"
    (goto-char 3)
    (should (eq 'gfm-links (gfm-links--xref-backend)))
    (goto-char (+ (point-min) 17))      ; inside the inline link
    (should-not (gfm-links--xref-backend))))

(ert-deftest lang-markdown/gfm-links-xref-jumps-to-definition ()
  "`xref-backend-definitions' points at the `[label]:' definition line."
  (lang-markdown-tests--with-links-buffer
      "[docs][adr] here.\n\n[adr]: https://adr.example\n"
    (goto-char 3)
    (let* ((id (xref-backend-identifier-at-point 'gfm-links))
           (defs (xref-backend-definitions 'gfm-links id))
           (def-pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "[adr]:")
                      (line-beginning-position))))
      (should (equal "adr" id))
      (should (= 1 (length defs)))
      (should (= def-pos
                 (xref-location-marker (xref-item-location (car defs))))))))

;;; 11.x Eldoc

(ert-deftest lang-markdown/gfm-links-eldoc-returns-url-on-link ()
  "Eldoc returns the resolved URL (and title attr) when point is on a link."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com \"official\")\n"
    (goto-char 3)
    (let ((doc (gfm-links--eldoc-function #'ignore)))
      (should (stringp doc))
      (should (string-match-p "https://anthropic.com" doc))
      (should (string-match-p "official" doc)))))

(ert-deftest lang-markdown/gfm-links-eldoc-returns-nil-off-link ()
  "Eldoc returns nil when point is not on a decorated link."
  (lang-markdown-tests--with-links-buffer
      "[Anthropic](https://anthropic.com) plain prose.\n"
    (goto-char (point-max))
    (should-not (gfm-links--eldoc-function #'ignore))))

;;; 14.8 Width walker — table cell with a decorated link

(ert-deftest lang-markdown/gfm-links-table-cell-width-counts-decoration ()
  "A table cell with a decorated link sizes its column by the visible width."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "| [Anthropic](https://anthropic.com) |\n| --- |\n| x |\n")
    (gfm-links-mode 1)
    (goto-char (point-min))
    (let* ((cells (gfm-tables--fontify-row-cells
                   (line-beginning-position) (line-end-position)))
           (w (gfm-tables--visible-width (car cells))))
      ;; "Anthropic" (9) + URL icon (1) = 10, well under the raw 34 chars.
      (should (<= w 12))
      (should (< w (length "[Anthropic](https://anthropic.com)"))))))

(ert-deftest lang-markdown/gfm-links-table-cell-bakes-in-decoration ()
  "A table cell's fontified string has the link decoration spliced in.
The raw bracket/URL text is gone — `display' text properties nested in
an overlay's `display' string are not honoured by redisplay, so the
decoration must be baked into the cell string itself."
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (insert "| [Anthropic](https://anthropic.com) |\n| --- |\n| x |\n")
    (gfm-links-mode 1)
    (goto-char (point-min))
    (let* ((cell (car (gfm-tables--fontify-row-cells
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

(ert-deftest lang-markdown/gfm-links-narrow-rebuild-widen-rebuild-converges ()
  "narrow -> rebuild -> widen -> rebuild matches a clean widened rebuild."
  :tags '(narrowing-regression)
  (with-temp-buffer
    (delay-mode-hooks (markdown-mode))
    (setq-local markdown-hide-urls t)
    (lang-markdown-tests--two-slide-links-buffer)
    (gfm-links-mode 1)
    (let* ((baseline (lang-markdown-tests--tagged-source-positions 'gfm-links))
           (slide-1-end (save-excursion
                          (goto-char (point-min))
                          (search-forward "# slide 2")
                          (line-beginning-position))))
      (narrow-to-region (point-min) slide-1-end)
      (gfm-links--rebuild)
      (widen)
      (gfm-links--rebuild)
      (should (equal baseline
                     (lang-markdown-tests--tagged-source-positions
                      'gfm-links))))))

(provide 'lang-markdown-tests)

;;; lang-markdown/tests.el ends here
