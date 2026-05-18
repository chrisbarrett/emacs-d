;;; lang-markdown/tests.el --- Tests for lang-markdown composition -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT suite for the lang-markdown composition layer.  Visual-behaviour
;; tests for the gfm-pretty engine live in `lisp/gfm/gfm-pretty-tests.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir))
       (lib-file (expand-file-name "lib.el" module-dir)))
  (condition-case nil
      (progn
        (load lib-file nil 'nomessage)
        (load init-file nil 'nomessage))
    (error nil)))

(ert-deftest lang-markdown/gfm-mode-remap ()
  "P1: markdown-mode should be remapped to gfm-mode or markdown-ts-mode."
  (should (memq (alist-get 'markdown-mode major-mode-remap-alist)
                '(gfm-mode markdown-ts-mode))))

(ert-deftest lang-markdown/prompt-file-association ()
  "P2: /prompt files should be associated with gfm-mode."
  (let ((entry (cl-find-if (lambda (e)
                             (and (stringp (car e))
                                  (string-match-p "prompt" (car e))))
                           auto-mode-alist)))
    (should (eq (cdr entry) 'gfm-mode))))

(ert-deftest lang-markdown/tempel-snippet-count ()
  "P9: There should be at least 10 snippets in markdown.eld."
  (let ((template-file (expand-file-name "templates/markdown.eld" user-emacs-directory)))
    (when (file-exists-p template-file)
      (with-temp-buffer
        (insert-file-contents template-file)
        (let ((count 0))
          (goto-char (point-min))
          (while (re-search-forward "^(\\w+" nil t)
            (setq count (1+ count)))
          (should (>= count 10)))))))

(ert-deftest lang-markdown/tab-dwim-defined ()
  "+markdown-tab-dwim is bound after loading lib.el."
  (should (fboundp '+markdown-tab-dwim)))

;;; tests.el ends here
