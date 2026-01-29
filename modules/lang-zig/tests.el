;;; tests.el --- Tests for lang-zig module -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the lang-zig module, covering testable properties P1-P6 from
;; specs/048-lang-zig.md.

;;; Code:

(require 'ert)

(let ((init-file (expand-file-name "init.el" (file-name-directory
                                               (or load-file-name buffer-file-name)))))
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil)))

;;; P1: *.zig files open in zig-mode

(ert-deftest lang-zig/zig-extension-mode ()
  "*.zig files should open in zig-mode."
  (let ((entry (assoc "\\.\\(zig\\|zon\\)\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'zig-mode))))

;;; P2: *.zon files open in zig-mode

(ert-deftest lang-zig/zon-extension-mode ()
  "*.zon files should open in zig-mode (same entry as *.zig)."
  (let ((entry (assoc "\\.\\(zig\\|zon\\)\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'zig-mode))))

;;; P4-P6: Tempel snippets available in zig-mode

(ert-deftest lang-zig/tempel-snippets ()
  "Tempel snippets should be defined for zig-mode.
Tests P4 (f), P5 (pf), and P6 (im) from the spec."
  (let ((templates-file (locate-file "zig.eld"
                                      (list (expand-file-name "templates" user-emacs-directory)))))
    (skip-unless templates-file)
    (with-temp-buffer
      (insert-file-contents templates-file)
      ;; File should have zig-mode heading and required snippets
      (should (search-forward "zig-mode" nil t))
      ;; P4: f snippet (function)
      (goto-char (point-min))
      (should (search-forward "(f " nil t))
      ;; P5: pf snippet (public function)
      (goto-char (point-min))
      (should (search-forward "(pf " nil t))
      ;; P6: im snippet (import)
      (goto-char (point-min))
      (should (search-forward "(im " nil t)))))

(provide 'lang-zig-tests)

;;; tests.el ends here
