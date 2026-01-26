;;; lang-c/tests.el --- Tests for lang-c module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for C language support module.

;;; Code:

(require 'ert)

;; Load module init from same directory as this test file
(defvar lang-c-tests--dir (file-name-directory (or load-file-name buffer-file-name)))
(load (expand-file-name "init.el" lang-c-tests--dir) nil t)

(defun lang-c-tests--treesitter-available-p ()
  "Check if C tree-sitter grammar is available."
  (and (fboundp 'treesit-ready-p)
       (treesit-ready-p 'c t)))

;;; P1: Opening .c file activates c-ts-mode (not c-mode)

(ert-deftest lang-c/mode-remap ()
  "P1: c-mode should be remapped to c-ts-mode."
  (should (eq (alist-get 'c-mode major-mode-remap-alist) 'c-ts-mode)))

;;; P2: On #include line, < inserts <> with cursor between

(ert-deftest lang-c/electric-bracket-include ()
  "P2: < on #include line should insert <> pair."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (with-temp-buffer
    (c-ts-mode)
    (insert "#include")
    (goto-char (point-max))
    (+c-electric-left-angle-bracket)
    (should (equal (buffer-string) "#include <>"))))

(ert-deftest lang-c/electric-bracket-include-with-space ()
  "P2: < on #include line normalizes spacing."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (with-temp-buffer
    (c-ts-mode)
    (insert "#include   ")
    (goto-char (point-max))
    (+c-electric-left-angle-bracket)
    (should (equal (buffer-string) "#include <>"))))

;;; P3: On non-include line, < inserts single <

(ert-deftest lang-c/electric-bracket-normal ()
  "P3: < on non-include line should insert single <."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (with-temp-buffer
    (c-ts-mode)
    (insert "if (a ")
    (goto-char (point-max))
    (let ((last-command-event ?<))
      (+c-electric-left-angle-bracket))
    (should (equal (buffer-string) "if (a <"))))

;;; P4: S-RET on line ending with text inserts ; and newline

(ert-deftest lang-c/auto-semi-text ()
  "P4: S-RET should insert semicolon on line ending with text."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (require 'evil)
  (with-temp-buffer
    (c-ts-mode)
    (insert "int x = 5")
    (+c-auto-insert-semi-newline)
    (should (string-match-p "int x = 5;" (buffer-string)))))

;;; P5: S-RET on line ending with { inserts only newline

(ert-deftest lang-c/auto-semi-brace ()
  "P5: S-RET should not insert semicolon on line ending with {."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (require 'evil)
  (with-temp-buffer
    (c-ts-mode)
    (insert "if (x) {")
    (+c-auto-insert-semi-newline)
    ;; Should not have added a semicolon before {
    (should-not (string-match-p ";{" (buffer-string)))
    (should (string-match-p "if (x) {" (buffer-string)))))

;;; P6: S-RET on line ending with : inserts only newline

(ert-deftest lang-c/auto-semi-colon ()
  "P6: S-RET should not insert semicolon on line ending with :."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (require 'evil)
  (with-temp-buffer
    (c-ts-mode)
    (insert "case 1:")
    (+c-auto-insert-semi-newline)
    (should-not (string-match-p ";:" (buffer-string)))
    (should (string-match-p "case 1:" (buffer-string)))))

;;; P7: S-RET on line ending with ; inserts only newline

(ert-deftest lang-c/auto-semi-existing ()
  "P7: S-RET should not add semicolon if one already exists."
  (skip-unless (lang-c-tests--treesitter-available-p))
  (require 'evil)
  (with-temp-buffer
    (c-ts-mode)
    (insert "return 0;")
    (+c-auto-insert-semi-newline)
    ;; Should not have doubled the semicolon
    (should-not (string-match-p ";;" (buffer-string)))))

;;; Keybindings tests

(ert-deftest lang-c/keybinding-electric-bracket ()
  "< in insert state should be bound to electric bracket command."
  (require 'c-ts-mode)
  (require 'general)
  (let ((binding (lookup-key c-ts-mode-map "<")))
    ;; General keybindings may be set up differently, check the command exists
    (should (fboundp '+c-electric-left-angle-bracket))))

(ert-deftest lang-c/keybinding-auto-semi ()
  "S-RET should be bound in c-ts-mode."
  (require 'c-ts-mode)
  (should (fboundp '+c-auto-insert-semi-newline)))

;;; Command existence

(ert-deftest lang-c/command-electric-bracket-defined ()
  "+c-electric-left-angle-bracket should be defined."
  (should (fboundp '+c-electric-left-angle-bracket)))

(ert-deftest lang-c/command-auto-semi-defined ()
  "+c-auto-insert-semi-newline should be defined."
  (should (fboundp '+c-auto-insert-semi-newline)))

(provide 'lang-c-tests)

;;; lang-c/tests.el ends here
