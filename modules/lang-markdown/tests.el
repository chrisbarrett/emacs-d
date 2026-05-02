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

;;; gfm-code-fences tests

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (fences-file (expand-file-name "lib/+gfm-code-fences.el" module-dir)))
  (when (file-exists-p fences-file)
    (load fences-file nil 'nomessage)))

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

(ert-deftest lang-markdown/gfm-code-fences-yaml-mode-prefers-treesit ()
  "Helmet language mode picks yaml-ts-mode when grammar available."
  (skip-unless (fboundp 'gfm-code-fences--yaml-mode))
  (skip-unless (and (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'yaml)))
  (should (eq 'yaml-ts-mode (gfm-code-fences--yaml-mode))))

(ert-deftest lang-markdown/gfm-code-fences-yaml-helmet-fontifies-body ()
  "YAML helmet body receives face overlays from the chosen yaml mode."
  (skip-unless (and (fboundp 'gfm-code-fences-mode)
                    (fboundp 'gfm-code-fences--yaml-mode)
                    (gfm-code-fences--yaml-mode)
                    (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'yaml)))
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
  (skip-unless (fboundp 'gfm-code-fences-mode))
  (with-temp-buffer
    (insert "---\n---\nbody\n")
    (should (progn (gfm-code-fences-mode 1) t))))

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

(ert-deftest lang-markdown/gfm-tables-edit-cell-command-defined ()
  (should (commandp 'gfm-tables-edit-cell-at-point)))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-strips-newlines ()
  (skip-unless (fboundp 'gfm-tables--cell-edit-sanitise))
  (with-temp-buffer
    (insert "a\nb\nc")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a b c" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-escapes-pipe ()
  (skip-unless (fboundp 'gfm-tables--cell-edit-sanitise))
  (with-temp-buffer
    (insert "a|b")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-keeps-existing-escape ()
  (skip-unless (fboundp 'gfm-tables--cell-edit-sanitise))
  (with-temp-buffer
    (insert "a\\|b")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "a\\|b" (buffer-string)))))

(ert-deftest lang-markdown/gfm-tables-cell-edit-sanitise-leading-pipe ()
  (skip-unless (fboundp 'gfm-tables--cell-edit-sanitise))
  (with-temp-buffer
    (insert "|a")
    (gfm-tables--cell-edit-sanitise)
    (should (equal "\\|a" (buffer-string)))))

;;; Cell bounds + active-cell highlight

(ert-deftest lang-markdown/gfm-tables-cell-bounds-simple ()
  (skip-unless (fboundp 'gfm-tables--cell-bounds))
  (with-temp-buffer
    (insert "| A | B |")
    (let ((cb (gfm-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb)))
      ;; First cell content begins right after the leading `|'.
      (should (eq ?| (char-before (car (nth 0 cb)))))
      (should (eq ?| (char-after  (cdr (nth 0 cb))))))))

(ert-deftest lang-markdown/gfm-tables-cell-bounds-honours-escape ()
  (skip-unless (fboundp 'gfm-tables--cell-bounds))
  (with-temp-buffer
    (insert "| a \\| b | c |")
    (let ((cb (gfm-tables--cell-bounds (point-min) (point-max))))
      (should (= 2 (length cb))))))

(ert-deftest lang-markdown/gfm-tables-cell-info-at-point ()
  (skip-unless (fboundp 'gfm-tables-mode))
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
  (skip-unless (fboundp 'gfm-tables-mode))
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
  (skip-unless (fboundp 'gfm-tables-mode))
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
  (skip-unless (fboundp 'gfm-tables-swap-column-right))
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
  (skip-unless (fboundp 'gfm-tables-swap-column-left))
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
  (skip-unless (fboundp 'gfm-tables-swap-column-right))
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
  (skip-unless (fboundp 'gfm-tables-cell-forward))
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
  (skip-unless (fboundp 'gfm-tables-cell-tab))
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
  (skip-unless (fboundp 'gfm-tables-cell-tab))
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
  (skip-unless (fboundp 'gfm-tables-cell-backtab))
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
  (skip-unless (fboundp 'gfm-tables-row-down))
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
  (skip-unless (fboundp 'gfm-tables-row-down))
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
  (skip-unless (fboundp 'gfm-tables-row-up))
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
  (skip-unless (fboundp 'gfm-tables--maybe-snap-to-cell))
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
  (skip-unless (fboundp 'gfm-tables--maybe-snap-to-cell))
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
  (skip-unless (fboundp 'gfm-tables--maybe-snap-to-cell))
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

;;; Evil shim

(ert-deftest lang-markdown/gfm-tables-evil-edit-commands-listed ()
  (should (boundp 'gfm-tables--evil-edit-commands))
  (should (memq 'evil-insert gfm-tables--evil-edit-commands))
  (should (memq 'evil-change gfm-tables--evil-edit-commands))
  (should (memq 'evil-open-below gfm-tables--evil-edit-commands)))

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

;;; Reveal

(provide 'lang-markdown-tests)

;;; lang-markdown/tests.el ends here
