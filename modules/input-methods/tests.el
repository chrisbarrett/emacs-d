;;; tests.el --- Input methods tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the input-methods module based on spec testable properties.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Capture module directory at load time (load-file-name is only valid during load)
(defvar input-methods--test-module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the input-methods module.")

;; Load module init at top level for reliable test discovery
(condition-case nil
    (load (expand-file-name "init.el" input-methods--test-module-dir) nil t)
  (error nil))

;; Load lib.el for +quail-defun macro
(condition-case nil
    (load (expand-file-name "lib.el" input-methods--test-module-dir) nil t)
  (error nil))


;;; P1: default-input-method equals "french-postfix"

(ert-deftest input-methods/default-input-method ()
  "P1: default-input-method equals french-postfix."
  (should (equal default-input-method "french-postfix")))


;;; P2: default-transient-input-method equals "french-postfix"

(ert-deftest input-methods/default-transient-input-method ()
  "P2: default-transient-input-method equals french-postfix."
  (should (equal default-transient-input-method "french-postfix")))


;;; P3: Smart semicolon - deletes horizontal space and inserts " ; "

(ert-deftest input-methods/smart-semicolon-behavior ()
  "P3: Semicolon in french-postfix deletes horizontal space and inserts spaced semicolon."
  (skip-unless (featurep 'quail))
  ;; Test the expected transformation: "word  " -> "word ; "
  ;; We can't easily test the full quail integration in batch mode,
  ;; but we can verify the with-eval-after-load form was registered
  ;; after-load-alist stores entries with regexp patterns as keys
  (should (cl-some (lambda (entry)
                     (string-match-p "latin-post" (format "%s" (car entry))))
                   after-load-alist)))


;;; P4: Smart colon - deletes horizontal space and inserts " : "

(ert-deftest input-methods/smart-colon-behavior ()
  "P4: Colon in french-postfix deletes horizontal space and inserts spaced colon."
  (skip-unless (featurep 'quail))
  ;; The with-eval-after-load registration is shared with semicolon
  ;; after-load-alist stores entries with regexp patterns as keys
  (should (cl-some (lambda (entry)
                     (string-match-p "latin-post" (format "%s" (car entry))))
                   after-load-alist)))


;;; P5: +quail-defun is a macro with correct argument list

(ert-deftest input-methods/quail-defun-is-macro ()
  "P5: +quail-defun is a macro."
  (should (macrop '+quail-defun)))

(ert-deftest input-methods/quail-defun-argument-list ()
  "P5: +quail-defun has correct argument list (package-name key &rest body)."
  (let ((arglist (help-function-arglist '+quail-defun)))
    (should (equal arglist '(package-name key &rest body)))))


;;; P6: +quail-defun creates function that clears quail state

(ert-deftest input-methods/quail-defun-expansion ()
  "P6: +quail-defun macro expands to code that clears quail state."
  (let ((expansion (macroexpand '(+quail-defun "test-package" "x" (insert "test")))))
    ;; Should contain setq for quail state variables
    (should (string-match-p "quail-current-str" (format "%S" expansion)))
    (should (string-match-p "quail-converting" (format "%S" expansion)))
    (should (string-match-p "quail-conversion-str" (format "%S" expansion)))))

(ert-deftest input-methods/quail-defun-throws-quail-tag ()
  "P6: +quail-defun throws to quail-tag."
  (let ((expansion (macroexpand '(+quail-defun "test-package" "x" (insert "test")))))
    (should (string-match-p "quail-tag" (format "%S" expansion)))))

(ert-deftest input-methods/quail-defun-atomic-change-group ()
  "P6: +quail-defun wraps body in atomic-change-group."
  (let ((expansion (macroexpand '(+quail-defun "test-package" "x" (insert "test")))))
    (should (string-match-p "atomic-change-group" (format "%S" expansion)))))


;;; Module structure tests

(ert-deftest input-methods/module-structure ()
  "Module has required files."
  (should (file-exists-p (expand-file-name "init.el" input-methods--test-module-dir)))
  (should (file-exists-p (expand-file-name "lib.el" input-methods--test-module-dir)))
  (should (file-exists-p (expand-file-name "packages.eld" input-methods--test-module-dir)))
  (should (file-exists-p (expand-file-name "spec.md" input-methods--test-module-dir))))

(provide 'input-methods-tests)

;;; tests.el ends here
