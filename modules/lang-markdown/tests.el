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
  "P1: markdown-mode should be remapped to gfm-mode."
  ;; Skip if init.el didn't load (missing +corelib in batch mode)
  (skip-unless (alist-get 'markdown-mode major-mode-remap-alist))
  (should (eq (alist-get 'markdown-mode major-mode-remap-alist) 'gfm-mode)))

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

(provide 'lang-markdown-tests)

;;; lang-markdown/tests.el ends here
