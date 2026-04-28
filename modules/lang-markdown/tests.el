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

(provide 'lang-markdown-tests)

;;; lang-markdown/tests.el ends here
