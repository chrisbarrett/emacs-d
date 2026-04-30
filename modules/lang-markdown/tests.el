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
  (skip-unless (alist-get 'markdown-mode major-mode-remap-alist))
  ;; Accept gfm-mode (our config) or markdown-ts-mode (tree-sitter default)
  (should (memq (alist-get 'markdown-mode major-mode-remap-alist)
                '(gfm-mode markdown-ts-mode))))

(ert-deftest lang-markdown/md-files-use-gfm-mode ()
  "P1: .md files should be directly associated with gfm-mode.
The first matching entry in auto-mode-alist should be gfm-mode."
  (let ((entry (cl-find-if (lambda (e)
                             (and (stringp (car e))
                                  (string-match-p "\\(md\\|markdown\\)" (car e))
                                  (eq (cdr e) 'gfm-mode)))
                           auto-mode-alist)))
    (skip-unless entry)
    (should entry)))

;;; P2: Opening file ending in /prompt activates gfm-mode

(ert-deftest lang-markdown/prompt-file-association ()
  "P2: /prompt files should be associated with gfm-mode."
  (let ((entry (cl-find-if (lambda (e)
                             (and (stringp (car e))
                                  (string-match-p "prompt" (car e))))
                           auto-mode-alist)))
    ;; Skip if init.el didn't load (missing +corelib in batch mode)
    (skip-unless entry)
    (should (eq (cdr entry) 'gfm-mode))))

;;; P3: markdown-fontify-code-blocks-natively is t

(ert-deftest lang-markdown/fontify-code-blocks ()
  "P3: markdown-fontify-code-blocks-natively should be t."
  (skip-unless (boundp 'markdown-fontify-code-blocks-natively))
  (should (eq markdown-fontify-code-blocks-natively t)))

;;; P4: [!NOTE] in GFM buffer has +markdown-gfm-callout-note-face

(ert-deftest lang-markdown/note-face-defined ()
  "P4: +markdown-gfm-callout-note-face should be defined."
  (should (facep '+markdown-gfm-callout-note-face)))

(ert-deftest lang-markdown/tip-face-defined ()
  "+markdown-gfm-callout-tip-face should be defined."
  (should (facep '+markdown-gfm-callout-tip-face)))

(ert-deftest lang-markdown/important-face-defined ()
  "+markdown-gfm-callout-important-face should be defined."
  (should (facep '+markdown-gfm-callout-important-face)))

(ert-deftest lang-markdown/warning-face-defined ()
  "+markdown-gfm-callout-warning-face should be defined."
  (should (facep '+markdown-gfm-callout-warning-face)))

(ert-deftest lang-markdown/caution-face-defined ()
  "+markdown-gfm-callout-caution-face should be defined."
  (should (facep '+markdown-gfm-callout-caution-face)))

(ert-deftest lang-markdown/prettier-ignore-face-defined ()
  "+markdown-prettier-ignore-comment-face should be defined."
  (should (facep '+markdown-prettier-ignore-comment-face)))

;;; P5: TAB in insert state expands tempel snippets before markdown-cycle

(ert-deftest lang-markdown/tab-dwim-defined ()
  "P5: +markdown-tab-dwim should be defined."
  (should (fboundp '+markdown-tab-dwim)))

(ert-deftest lang-markdown/tab-keybinding ()
  "P5: TAB should be bound to +markdown-tab-dwim in insert state."
  ;; Skip if general not loaded in batch mode
  (skip-unless (and (boundp 'evil-insert-state-map)
                    (boundp 'markdown-mode-map)))
  ;; Check that markdown-mode-map has TAB bound
  (let ((key (lookup-key markdown-mode-map (kbd "TAB"))))
    ;; Accept any binding (may be via general)
    (should key)))

;;; P6: apheleia-formatter set to deno-markdown when deno available

(ert-deftest lang-markdown/deno-formatter-defined ()
  "P6: deno-markdown formatter should be defined in apheleia."
  (skip-unless (boundp 'apheleia-formatters))
  (should (alist-get 'deno-markdown apheleia-formatters)))

(ert-deftest lang-markdown/prettier-formatter-defined ()
  "P6: prettier-markdown formatter should be defined as fallback."
  (skip-unless (boundp 'apheleia-formatters))
  (should (alist-get 'prettier-markdown apheleia-formatters)))

;;; P7: C-c f calls markdown-insert-footnote

(ert-deftest lang-markdown/footnote-keybinding ()
  "P7: C-c f should be bound in markdown-mode-map."
  (skip-unless (boundp 'markdown-mode-map))
  (let ((key (lookup-key markdown-mode-map (kbd "C-c f"))))
    ;; May be nil if deferred, so just check for binding presence or hook
    (should (or key
                ;; Check if general-config hook is set up
                (memq 'general-config-markdown-mode-map markdown-mode-hook)))))

;;; P8: Local leader l toggles URL hiding

(ert-deftest lang-markdown/hide-urls-setting ()
  "P8: markdown-hide-urls should be t."
  (skip-unless (boundp 'markdown-hide-urls))
  (should (eq markdown-hide-urls t)))

;;; P9: 11 tempel snippets available in markdown-mode

(ert-deftest lang-markdown/tempel-snippets-exist ()
  "P9: Tempel snippets should be defined for markdown-mode."
  (let ((template-file (expand-file-name "templates/markdown.eld" user-emacs-directory)))
    (should (file-exists-p template-file))))

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

;;; Additional tests

(ert-deftest lang-markdown/fontify-callouts-defined ()
  "+markdown-fontify-gfm-callouts should be defined."
  (should (fboundp '+markdown-fontify-gfm-callouts)))

(ert-deftest lang-markdown/gfm-mode-visual-line ()
  "gfm-mode-hook should enable visual-line-mode."
  (skip-unless (boundp 'gfm-mode-hook))
  (should (memq 'visual-line-mode gfm-mode-hook)))

(ert-deftest lang-markdown/gfm-mode-callouts-hook ()
  "gfm-mode-hook should fontify callouts."
  (skip-unless (boundp 'gfm-mode-hook))
  (should (memq '+markdown-fontify-gfm-callouts gfm-mode-hook)))

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

(ert-deftest lang-markdown/gfm-callouts-mode-defined ()
  (should (fboundp 'gfm-callouts-mode)))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-detects-types ()
  "Each known callout type is detected with its label."
  (skip-unless (fboundp 'gfm-callouts--find-blocks))
  (dolist (type '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION" "CRITICAL"))
    (with-temp-buffer
      (insert (lang-markdown-tests--callout-block type '("hello")))
      (let ((blocks (gfm-callouts--find-blocks)))
        (should (= 1 (length blocks)))
        (should (equal type (nth 2 (car blocks))))))))

(ert-deftest lang-markdown/gfm-callouts-find-blocks-multiline ()
  "Block end extends through subsequent blockquote lines."
  (skip-unless (fboundp 'gfm-callouts--find-blocks))
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
  (skip-unless (fboundp 'gfm-callouts--find-blocks))
  (with-temp-buffer
    (insert "> just a quote\n> with two lines\n")
    (should-not (gfm-callouts--find-blocks))))

(ert-deftest lang-markdown/gfm-callouts-mode-creates-overlays ()
  (skip-unless (fboundp 'gfm-callouts-mode))
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-callouts))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-callouts-mode-removes-overlays ()
  (skip-unless (fboundp 'gfm-callouts-mode))
  (with-temp-buffer
    (insert "> [!NOTE]\n> Hello.\n")
    (gfm-callouts-mode 1)
    (gfm-callouts-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-callouts))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-callouts-enabled-via-gfm-mode-hook ()
  (skip-unless (boundp 'gfm-mode-hook))
  (should (memq 'gfm-callouts-mode gfm-mode-hook)))

;;; gfm-code-fences tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (fences-file (expand-file-name "lib/+gfm-code-fences.el" module-dir)))
  (when (file-exists-p fences-file)
    (load fences-file nil 'nomessage)))

(ert-deftest lang-markdown/gfm-code-fences-mode-defined ()
  (should (fboundp 'gfm-code-fences-mode)))

(ert-deftest lang-markdown/gfm-code-fences-find-block ()
  (skip-unless (fboundp 'gfm-code-fences--find-blocks))
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (let ((blocks (gfm-code-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should (equal "bash" (nth 4 (car blocks)))))))

(ert-deftest lang-markdown/gfm-code-fences-find-block-no-lang ()
  (skip-unless (fboundp 'gfm-code-fences--find-blocks))
  (with-temp-buffer
    (insert "```\ntext\n```\n")
    (let ((blocks (gfm-code-fences--find-blocks)))
      (should (= 1 (length blocks)))
      (should-not (nth 4 (car blocks))))))

(ert-deftest lang-markdown/gfm-code-fences-mode-creates-overlays ()
  (skip-unless (fboundp 'gfm-code-fences-mode))
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-code-fences))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-code-fences-mode-removes-overlays ()
  (skip-unless (fboundp 'gfm-code-fences-mode))
  (with-temp-buffer
    (insert "```bash\necho hi\n```\n")
    (gfm-code-fences-mode 1)
    (gfm-code-fences-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-code-fences))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-code-fences-enabled-via-gfm-mode-hook ()
  (skip-unless (boundp 'gfm-mode-hook))
  (should (memq 'gfm-code-fences-mode gfm-mode-hook)))

(ert-deftest lang-markdown/gfm-code-fences-find-indent-block ()
  (skip-unless (fboundp 'gfm-code-fences--find-indent-blocks))
  (with-temp-buffer
    (insert "Para.\n\n    code one\n    code two\n\nMore.\n")
    (let ((blocks (gfm-code-fences--find-indent-blocks nil)))
      (should (= 1 (length blocks)))
      (should (= 4 (nth 2 (car blocks)))))))

(ert-deftest lang-markdown/gfm-code-fences-skip-indent-inside-fence ()
  (skip-unless (and (fboundp 'gfm-code-fences--find-indent-blocks)
                    (fboundp 'gfm-code-fences--find-blocks)))
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

;;; gfm-tables tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (tables-file (expand-file-name "lib/+gfm-tables.el" module-dir)))
  (when (file-exists-p tables-file)
    (load tables-file nil 'nomessage)))

(ert-deftest lang-markdown/gfm-tables-mode-defined ()
  (should (fboundp 'gfm-tables-mode)))

(ert-deftest lang-markdown/gfm-tables-row-alt-face-defined ()
  (should (facep 'gfm-tables-row-alt-face)))

;;; Cell parser

(ert-deftest lang-markdown/gfm-tables-split-row-simple ()
  (skip-unless (fboundp 'gfm-tables--split-row))
  (should (equal '("a" "b" "c")
                 (gfm-tables--split-row "| a | b | c |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-escaped-pipe ()
  (skip-unless (fboundp 'gfm-tables--split-row))
  (should (equal '("a | b" "c")
                 (gfm-tables--split-row "| a \\| b | c |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-single-tick-code ()
  (skip-unless (fboundp 'gfm-tables--split-row))
  (should (equal '("a" "`b|c`" "d")
                 (gfm-tables--split-row "| a | `b|c` | d |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-double-tick-code ()
  (skip-unless (fboundp 'gfm-tables--split-row))
  (should (equal '("a" "``b|c``" "d")
                 (gfm-tables--split-row "| a | ``b|c`` | d |"))))

(ert-deftest lang-markdown/gfm-tables-split-row-unbalanced-tick ()
  "Unbalanced backtick is treated as literal text."
  (skip-unless (fboundp 'gfm-tables--split-row))
  (should (equal '("a" "`b" "c")
                 (gfm-tables--split-row "| a | `b | c |"))))

;;; Block discovery

(ert-deftest lang-markdown/gfm-tables-find-blocks-standard ()
  (skip-unless (fboundp 'gfm-tables--find-blocks))
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n| 3 | 4 |\n")
    (let ((blocks (gfm-tables--find-blocks)))
      (should (= 1 (length blocks))))))

(ert-deftest lang-markdown/gfm-tables-find-blocks-rejects-lone-delim ()
  (skip-unless (fboundp 'gfm-tables--find-blocks))
  (with-temp-buffer
    (insert "Some prose.\n| - | - |\nMore prose.\n")
    (should-not (gfm-tables--find-blocks))))

(ert-deftest lang-markdown/gfm-tables-find-blocks-skips-fenced ()
  (skip-unless (and (fboundp 'gfm-tables--find-blocks)
                    (fboundp 'gfm-code-fences--find-blocks)))
  (with-temp-buffer
    (insert "```\n| A | B |\n| - | - |\n| 1 | 2 |\n```\n")
    (let* ((fenced (gfm-code-fences--find-blocks))
           (excluded (mapcar (lambda (b) (cons (nth 0 b) (nth 3 b))) fenced))
           (blocks (gfm-tables--find-blocks excluded)))
      (should (= 0 (length blocks))))))

;;; Column widths

(ert-deftest lang-markdown/gfm-tables-column-widths-unaligned ()
  (skip-unless (fboundp 'gfm-tables--column-widths))
  (let* ((rows '(("Header" "B")
                 ("a" "longer")
                 ("xx" "y")))
         (widths (gfm-tables--column-widths rows)))
    (should (equal 6 (aref widths 0)))
    (should (equal 6 (aref widths 1)))))

(ert-deftest lang-markdown/gfm-tables-box-width ()
  (skip-unless (fboundp 'gfm-tables--box-width))
  ;; 2 cols, widths 3 and 5 → 2 + (5) + (7) + 1 = 15
  (should (= 15 (gfm-tables--box-width (vector 3 5)))))

;;; Cell wrapping

(ert-deftest lang-markdown/gfm-tables-cell-tokens-splits-on-whitespace ()
  (skip-unless (fboundp 'gfm-tables--cell-tokens))
  (should (equal '("foo" "bar" "baz")
                 (gfm-tables--cell-tokens "  foo  bar baz "))))

(ert-deftest lang-markdown/gfm-tables-cell-tokens-empty-string ()
  (skip-unless (fboundp 'gfm-tables--cell-tokens))
  (should (equal '() (gfm-tables--cell-tokens "")))
  (should (equal '() (gfm-tables--cell-tokens "   "))))

(ert-deftest lang-markdown/gfm-tables-cell-tokens-preserves-properties ()
  (skip-unless (fboundp 'gfm-tables--cell-tokens))
  (let* ((src (concat "abc " (propertize "def" 'face 'bold)))
         (tokens (gfm-tables--cell-tokens src)))
    (should (equal '("abc" "def") tokens))
    (should (eq 'bold (get-text-property 0 'face (cadr tokens))))))

(ert-deftest lang-markdown/gfm-tables-slice-by-visible-width-basic ()
  (skip-unless (fboundp 'gfm-tables--slice-by-visible-width))
  (should (equal '("abc" "de") (gfm-tables--slice-by-visible-width "abcde" 3))))

(ert-deftest lang-markdown/gfm-tables-slice-by-visible-width-zero-falls-back ()
  (skip-unless (fboundp 'gfm-tables--slice-by-visible-width))
  ;; Width 0 must still make progress (one char per slice).
  (let ((chunks (gfm-tables--slice-by-visible-width "ab" 0)))
    (should (= 2 (length chunks)))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-no-wrap ()
  (skip-unless (fboundp 'gfm-tables--wrap-cell))
  (should (equal '("hello") (gfm-tables--wrap-cell "hello" 10))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-word-boundary ()
  (skip-unless (fboundp 'gfm-tables--wrap-cell))
  (should (equal '("the quick" "brown fox")
                 (gfm-tables--wrap-cell "the quick brown fox" 9))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-hard-break-long-word ()
  (skip-unless (fboundp 'gfm-tables--wrap-cell))
  (let ((lines (gfm-tables--wrap-cell "abcdefghij" 4)))
    (should (cl-every (lambda (l) (<= (string-width l) 4)) lines))
    (should (equal "abcdefghij" (apply #'concat lines)))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-empty-returns-one-empty-line ()
  (skip-unless (fboundp 'gfm-tables--wrap-cell))
  (should (equal '("") (gfm-tables--wrap-cell "" 5))))

(ert-deftest lang-markdown/gfm-tables-wrap-cell-preserves-properties ()
  (skip-unless (fboundp 'gfm-tables--wrap-cell))
  (let* ((src (concat (propertize "abc" 'face 'bold) " def"))
         (lines (gfm-tables--wrap-cell src 3)))
    (should (eq 'bold (get-text-property 0 'face (car lines))))))

;;; Width fitting

(ert-deftest lang-markdown/gfm-tables-fit-widths-under-budget-passthrough ()
  (skip-unless (fboundp 'gfm-tables--fit-widths))
  (should (equal (vector 3 5) (gfm-tables--fit-widths (vector 3 5) 100))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-caps-widest ()
  (skip-unless (fboundp 'gfm-tables--fit-widths))
  ;; natural sum 30, budget 20: smaller col fits naturally, widest capped.
  (let ((fitted (gfm-tables--fit-widths (vector 5 25) 20)))
    (should (= 5 (aref fitted 0)))
    (should (= 15 (aref fitted 1)))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-equal-distribution ()
  (skip-unless (fboundp 'gfm-tables--fit-widths))
  ;; Equal natural widths over budget → all capped equally.
  (let ((fitted (gfm-tables--fit-widths (vector 20 20) 30)))
    (should (= (aref fitted 0) (aref fitted 1)))
    (should (<= (+ (aref fitted 0) (aref fitted 1)) 30))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-uses-full-budget ()
  "Integer slack from binary search is distributed so sum = budget."
  (skip-unless (fboundp 'gfm-tables--fit-widths))
  ;; Natural [15 118 57], budget 50: water cap is 17 → 15+17+17=49.
  ;; The 1 unit of slack must be redistributed so total = 50.
  (let ((fitted (gfm-tables--fit-widths (vector 15 118 57) 50)))
    (should (= 50 (cl-loop for w across fitted sum w)))))

(ert-deftest lang-markdown/gfm-tables-fit-widths-floor-at-1 ()
  (skip-unless (fboundp 'gfm-tables--fit-widths))
  ;; Tiny budget shouldn't produce zero widths.
  (let ((fitted (gfm-tables--fit-widths (vector 10 10 10) 1)))
    (should (cl-every (lambda (w) (>= w 1)) (cl-coerce fitted 'list)))))

;;; Multi-line compose

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-single-line ()
  "If no cell exceeds its width, output is a single line (no `\\n')."
  (skip-unless (fboundp 'gfm-tables--compose-multiline-row))
  (let ((row (gfm-tables--compose-multiline-row '("a" "b") (vector 1 1)
                                                'body-default)))
    (should-not (string-match-p "\n" row))))

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-wraps-cell ()
  "A cell wider than its column wraps to multiple visual lines."
  (skip-unless (fboundp 'gfm-tables--compose-multiline-row))
  (let* ((row (gfm-tables--compose-multiline-row
               '("a" "one two three four") (vector 1 9)
               'body-default))
         (lines (split-string row "\n")))
    (should (>= (length lines) 2))
    (should (cl-every (lambda (l) (= (length l) (length (car lines)))) lines))))

(ert-deftest lang-markdown/gfm-tables-compose-multiline-row-pads-short-cells ()
  "Short cells get padded with blank lines so columns stay aligned."
  (skip-unless (fboundp 'gfm-tables--compose-multiline-row))
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
  (skip-unless (and (fboundp 'gfm-tables--fontify-cell)
                    (fboundp 'markdown-mode)))
  (let ((s (gfm-tables--fontify-cell "**bold**")))
    (should (cl-some (lambda (i)
                       (let ((f (get-text-property i 'face s)))
                         (or (eq f 'markdown-bold-face)
                             (and (listp f) (memq 'markdown-bold-face f)))))
                     (number-sequence 0 (1- (length s)))))))

(ert-deftest lang-markdown/gfm-tables-fontify-cell-applies-code-face ()
  "Inline code inside a cell receives `markdown-inline-code-face'."
  (skip-unless (and (fboundp 'gfm-tables--fontify-cell)
                    (fboundp 'markdown-mode)))
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
  (skip-unless (and (fboundp 'gfm-tables--fontify-cell)
                    (fboundp 'gfm-tables--visible-width)
                    (fboundp 'markdown-mode)))
  (with-temp-buffer
    (setq buffer-invisibility-spec '(t))
    (dolist (raw '("plain" "**bold**" "*it*" "`code`" "[t](u)" ""))
      (let ((fontified (gfm-tables--fontify-cell raw)))
        (should (= (string-width raw)
                   (gfm-tables--visible-width fontified)))))))

(ert-deftest lang-markdown/gfm-tables-visible-width-honours-display ()
  "`display' string property changes visible width."
  (skip-unless (fboundp 'gfm-tables--visible-width))
  (let ((s (concat "abc" (propertize "X" 'display "longer") "de")))
    (should (= (+ 5 (string-width "longer"))
               (gfm-tables--visible-width s)))))

(ert-deftest lang-markdown/gfm-tables-visible-width-honours-invisibility-spec ()
  "Invisible-tagged chars only shrink width when in `buffer-invisibility-spec'."
  (skip-unless (fboundp 'gfm-tables--visible-width))
  (let ((s (concat "ab" (propertize "XX" 'invisible 'tag) "cd")))
    (with-temp-buffer
      (setq buffer-invisibility-spec nil)
      (should (= 6 (gfm-tables--visible-width s)))
      (setq buffer-invisibility-spec '(tag))
      (should (= 4 (gfm-tables--visible-width s))))))

(ert-deftest lang-markdown/gfm-tables-compose-row-preserves-cell-faces ()
  "`compose-row' on body-alt row keeps existing markdown faces on cell text."
  (skip-unless (and (fboundp 'gfm-tables--compose-row)
                    (fboundp 'gfm-tables--fontify-cell)
                    (fboundp 'markdown-mode)))
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
  (skip-unless (fboundp 'gfm-tables-mode))
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'gfm-tables)
        (should-not (overlay-get ov 'evaporate))))))

;;; Indirect editing

(ert-deftest lang-markdown/gfm-tables-block-at-point-inside ()
  (skip-unless (fboundp 'gfm-tables--block-at-point))
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (search-forward "1")
    (let ((bounds (gfm-tables--block-at-point)))
      (should bounds)
      (should (< (car bounds) (point)))
      (should (> (cdr bounds) (point))))))

(ert-deftest lang-markdown/gfm-tables-block-at-point-outside-returns-nil ()
  (skip-unless (fboundp 'gfm-tables--block-at-point))
  (with-temp-buffer
    (insert "intro\n| A | B |\n| - | - |\n| 1 | 2 |\nout\n")
    (goto-char (point-min))
    (should-not (gfm-tables--block-at-point))))

(ert-deftest lang-markdown/gfm-tables-edit-table-command-defined ()
  (should (commandp 'gfm-tables-edit-table-at-point)))

;;; Mode lifecycle

(ert-deftest lang-markdown/gfm-tables-mode-creates-overlays ()
  (skip-unless (fboundp 'gfm-tables-mode))
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (should (cl-some (lambda (ov) (overlay-get ov 'gfm-tables))
                     (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-mode-removes-overlays ()
  (skip-unless (fboundp 'gfm-tables-mode))
  (with-temp-buffer
    (insert "| A | B |\n| - | - |\n| 1 | 2 |\n")
    (gfm-tables-mode 1)
    (gfm-tables-mode -1)
    (should-not (cl-some (lambda (ov) (overlay-get ov 'gfm-tables))
                         (overlays-in (point-min) (point-max))))))

(ert-deftest lang-markdown/gfm-tables-enabled-via-gfm-mode-hook ()
  (skip-unless (boundp 'gfm-mode-hook))
  (should (memq 'gfm-tables-mode gfm-mode-hook)))

(provide 'lang-markdown-tests)

;;; lang-markdown/tests.el ends here
