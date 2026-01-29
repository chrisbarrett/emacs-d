;;; tests.el --- Tests for search module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the search module, covering the spec's testable properties.

;;; Code:

(require 'ert)

(defun search-test--load-init ()
  "Load the search module init.el."
  (load (expand-file-name "modules/search/init.el" user-emacs-directory) nil t))

;; P2: grep-template contains "rg" command
(ert-deftest search-module-test-p2-grep-template-uses-rg ()
  "P2: grep-template should use ripgrep."
  (search-test--load-init)
  (require 'grep)
  (should (string-match-p "\\brg\\b" grep-template)))

;; P5: Opening occur buffer enables hl-line-mode
(ert-deftest search-module-test-p5-occur-hl-line-mode ()
  "P5: occur-mode-hook should include hl-line-mode."
  (search-test--load-init)
  (require 'replace)
  (should (memq #'hl-line-mode occur-mode-hook)))

;; Additional tests for grep-template details

(ert-deftest search-module-test-grep-template-line-number ()
  "grep-template should include --line-number flag."
  (search-test--load-init)
  (require 'grep)
  (should (string-match-p "--line-number" grep-template)))

(ert-deftest search-module-test-grep-template-with-filename ()
  "grep-template should include --with-filename flag."
  (search-test--load-init)
  (require 'grep)
  (should (string-match-p "--with-filename" grep-template)))

(ert-deftest search-module-test-grep-template-null ()
  "grep-template should include --null flag."
  (search-test--load-init)
  (require 'grep)
  (should (string-match-p "--null" grep-template)))

(provide 'search-tests)

;;; tests.el ends here
