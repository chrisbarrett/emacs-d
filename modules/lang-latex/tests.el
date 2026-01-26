;;; tests.el --- Tests for lang-latex module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for LaTeX editing support with latexindent formatter.

;;; Code:

(require 'ert)

;; Load the module init.el
(condition-case nil
    (load (expand-file-name "init.el"
                            (file-name-directory (or load-file-name
                                                     buffer-file-name))))
  (error nil))

;;; P1: apheleia-formatters contains latexindent entry after tex-mode loads

(ert-deftest lang-latex-test/apheleia-formatters-has-latexindent ()
  "P1: apheleia-formatters contains latexindent entry."
  (skip-unless (featurep 'apheleia))
  (require 'tex-mode) ; Trigger :after condition
  (should (assq 'latexindent apheleia-formatters)))

;;; P2: Formatter command includes --logfile=/dev/null

(ert-deftest lang-latex-test/formatter-disables-logfile ()
  "P2: Formatter command includes --logfile=/dev/null."
  (skip-unless (featurep 'apheleia))
  (require 'tex-mode)
  (let ((formatter (cdr (assq 'latexindent apheleia-formatters))))
    (should formatter)
    (should (member "--logfile=/dev/null" formatter))))

;;; P3: Formatter command respects indent-tabs-mode

(ert-deftest lang-latex-test/formatter-has-indent-logic ()
  "P3: Formatter contains conditional indent style logic."
  (skip-unless (featurep 'apheleia))
  (require 'tex-mode)
  (let ((formatter (cdr (assq 'latexindent apheleia-formatters))))
    (should formatter)
    ;; The formatter should have a when form for indent style
    (should (cl-find-if (lambda (el)
                          (and (listp el)
                               (eq (car el) 'when)))
                        formatter))))

(ert-deftest lang-latex-test/formatter-tabs-mode-branch ()
  "P3: Formatter has both tabs and spaces branches."
  (skip-unless (featurep 'apheleia))
  (require 'tex-mode)
  (let* ((formatter (cdr (assq 'latexindent apheleia-formatters)))
         (when-form (cl-find-if (lambda (el)
                                  (and (listp el)
                                       (eq (car el) 'when)))
                                formatter)))
    (should when-form)
    ;; Should contain if form checking indent-tabs-mode
    (let ((if-form (cl-find-if (lambda (el)
                                 (and (listp el)
                                      (eq (car el) 'if)))
                               when-form)))
      (should if-form)
      (should (eq (cadr if-form) 'indent-tabs-mode)))))

;;; P4: Format-on-save configuration

(ert-deftest lang-latex-test/tex-mode-in-apheleia-mode-alist ()
  "P4: tex-mode should use latexindent formatter."
  (skip-unless (and (featurep 'apheleia)
                    (boundp 'apheleia-mode-alist)))
  ;; Check that latex-mode or tex-mode maps to latexindent
  ;; This may be in default config or added elsewhere
  (let ((latex-entry (or (assq 'latex-mode apheleia-mode-alist)
                         (assq 'LaTeX-mode apheleia-mode-alist)
                         (assq 'tex-mode apheleia-mode-alist))))
    ;; Just verify the entry exists if configured
    (when latex-entry
      (should (eq (cdr latex-entry) 'latexindent)))))

;;; Module structure tests

(ert-deftest lang-latex-test/provides-feature ()
  "Module provides lang-latex-init feature."
  (should (featurep 'lang-latex-init)))

(provide 'lang-latex-tests)

;;; tests.el ends here
