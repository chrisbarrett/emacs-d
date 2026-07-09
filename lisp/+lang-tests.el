;;; +lang-tests.el --- Tests for the +lang-declare helper -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the declarative language-wiring helper `+lang-declare'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require '+lang)

;;; Laziness: declaring must not force eglot or apheleia to load

(ert-deftest +lang/declare-does-not-force-load ()
  "Declaring wiring must not `require' eglot or apheleia."
  (let ((required '()))
    (cl-letf* ((orig (symbol-function 'require))
               ((symbol-function 'require)
                (lambda (feature &rest args)
                  (push feature required)
                  (apply orig feature args))))
      (+lang-declare '+lang-test-lazy-mode
                     :lsp t
                     :formatter '(+lang-test-lazy-tool . ("cat" "-"))))
    (should-not (memq 'eglot required))
    (should-not (memq 'apheleia required))))

;;; LSP activation attaches to the local-vars hook

(ert-deftest +lang/lsp-adds-local-vars-hook ()
  "LSP declaration adds `eglot-ensure' to the mode's local-vars hook."
  (defvar +lang-test-lsp-mode-local-vars-hook nil)
  (setq +lang-test-lsp-mode-local-vars-hook nil)
  (+lang-declare '+lang-test-lsp-mode :lsp t)
  (should (memq 'eglot-ensure +lang-test-lsp-mode-local-vars-hook)))

;;; Formatter pair registers definition and association together

(ert-deftest +lang/formatter-pair-registers-both ()
  "A (NAME . COMMAND) formatter registers definition and association."
  (skip-unless (require 'apheleia nil t))
  (+lang-declare '+lang-test-pair-mode
                 :formatter '(+lang-test-tool . ("+lang-test-tool" "-")))
  (should (equal (alist-get '+lang-test-tool apheleia-formatters)
                 '("+lang-test-tool" "-")))
  (should (eq (alist-get '+lang-test-pair-mode apheleia-mode-alist)
              '+lang-test-tool)))

;;; A declaration naming an existing formatter adds only the association

(ert-deftest +lang/formatter-symbol-associates-only ()
  "A symbol formatter adds only the mode association."
  (skip-unless (require 'apheleia nil t))
  (let ((before (copy-alist apheleia-formatters)))
    (+lang-declare '+lang-test-sym-mode :formatter 'ocamlformat)
    (should (eq (alist-get '+lang-test-sym-mode apheleia-mode-alist)
                'ocamlformat))
    (should (equal (alist-get 'ocamlformat apheleia-formatters)
                   (alist-get 'ocamlformat before)))))

;;; A nil mode registers a definition without a mode association

(ert-deftest +lang/formatter-define-only ()
  "A nil MODE registers the formatter without a mode association."
  (skip-unless (require 'apheleia nil t))
  (+lang-declare nil :formatter '(+lang-test-defonly . ("x" "-")))
  (should (equal (alist-get '+lang-test-defonly apheleia-formatters)
                 '("x" "-")))
  (should-not (rassq '+lang-test-defonly apheleia-mode-alist)))

;;; Enforcement: language modules wire through the helper, not by hand

(defconst +lang-test--enforcement-exceptions
  '("lang-terraform")  ; genuine variation: hcl-mode maps to a formatter
                       ; chain (terragrunt hclfmt) that `+lang-declare'
                       ; cannot express, so its wiring stays hand-rolled
  "Module basenames exempt from the no-hand-rolled-wiring check.")

(ert-deftest +lang/no-hand-rolled-wiring-remains ()
  "No lang-* init.el hand-wires eglot-ensure hooks or apheleia alists.
Such wiring must go through `+lang-declare'.  Exempt modules are listed,
with rationale, in `+lang-test--enforcement-exceptions'."
  (let* ((root (locate-dominating-file
                (or load-file-name buffer-file-name default-directory)
                "modules"))
         (modules-dir (expand-file-name "modules" root))
         (eglot-re (rx "local-vars-hook" (* (any " \t\n")) "."
                       (* (any " \t\n")) "eglot-ensure" symbol-end))
         (apheleia-re (rx "(" (or "add-to-list" "alist-set!" "push" "setq" "setf")
                          (* (not (any ")"))) "apheleia-" (or "formatters" "mode-alist")
                          symbol-end))
         (offenders '()))
    (dolist (init (directory-files-recursively modules-dir "\\`init\\.el\\'"))
      (let ((module (file-name-nondirectory
                     (directory-file-name (file-name-directory init)))))
        (when (and (string-prefix-p "lang-" module)
                   (not (member module +lang-test--enforcement-exceptions)))
          (with-temp-buffer
            (insert-file-contents init)
            (goto-char (point-min))
            (when (or (re-search-forward eglot-re nil t)
                      (progn (goto-char (point-min))
                             (re-search-forward apheleia-re nil t)))
              (push (file-relative-name init root) offenders))))))
    (should-not offenders)))

(provide '+lang-tests)

;;; +lang-tests.el ends here
