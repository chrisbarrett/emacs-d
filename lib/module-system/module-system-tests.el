;;; module-system-tests.el --- Tests for module-system.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the module system.
;; Tests are organized into:
;; - Filesystem traversal tests (using temporary directories)
;; - Data parsing tests (packages.eld, autoloads)
;; - Autoload generation tests

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module under test
(let ((module-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path module-dir)
  (require 'module-system))


;;; Test Helpers

(defmacro module-system-test-with-temp-features (features &rest body)
  "Create a temporary features directory with FEATURES and execute BODY.
FEATURES is a list of (slug . files) where files is an alist of
\(filename . contents) pairs.

Binds `temp-features-dir' to the temporary directory path."
  (declare (indent 1) (debug (form body)))
  `(let ((temp-features-dir (make-temp-file "module-system-test" t)))
     (unwind-protect
         (progn
           ;; Create feature directories and files
           (dolist (feature ,features)
             (let* ((slug (car feature))
                    (files (cdr feature))
                    (feature-dir (expand-file-name slug temp-features-dir)))
               (make-directory feature-dir t)
               (dolist (file files)
                 (let* ((filename (car file))
                        (contents (cdr file))
                        (filepath (expand-file-name filename feature-dir))
                        (parent-dir (file-name-directory filepath)))
                   (when parent-dir
                     (make-directory parent-dir t))
                   (with-temp-file filepath
                     (insert contents))))))
           ,@body)
       ;; Cleanup
       (delete-directory temp-features-dir t))))


;;; Filesystem Traversal Tests

(ert-deftest module-system--discover-all--empty-directory ()
  "Discovery returns empty list for empty features directory."
  (module-system-test-with-temp-features nil
    (should (null (module-system-discover-all temp-features-dir)))))

(ert-deftest module-system--discover-all--finds-feature-directories ()
  "Discovery finds all feature directories."
  (module-system-test-with-temp-features
      '(("feature-a" . (("spec.md" . "# Feature A")))
        ("feature-b" . (("spec.md" . "# Feature B")))
        ("feature-c" . (("spec.md" . "# Feature C"))))
    (let ((modules (module-system-discover-all temp-features-dir)))
      (should (= 3 (length modules)))
      (should (cl-every #'module-system-module-p modules))
      (let ((slugs (mapcar #'module-system-module-slug modules)))
        (should (member "feature-a" slugs))
        (should (member "feature-b" slugs))
        (should (member "feature-c" slugs))))))

(ert-deftest module-system--discover-module--all-files-present ()
  "Discovery correctly identifies all feature files."
  (module-system-test-with-temp-features
      '(("complete-feature" .
         (("spec.md" . "# Spec")
          ("packages.eld" . "((some-package))")
          ("init.el" . "(message \"init\")")
          ("lib.el" . ";;;###autoload\n(defun complete-func () nil)")
          ("tests.el" . "(ert-deftest test-1 () t)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (mod (car modules)))
      (should (= 1 (length modules)))
      (should (equal "complete-feature" (module-system-module-slug mod)))
      (should (module-system-module-spec-file mod))
      (should (module-system-module-packages-file mod))
      (should (module-system-module-init-file mod))
      (should (= 1 (length (module-system-module-lib-files mod))))
      (should (module-system-module-tests-file mod)))))

(ert-deftest module-system--discover-module--partial-files ()
  "Discovery handles features with only some files."
  (module-system-test-with-temp-features
      '(("minimal-feature" .
         (("init.el" . "(message \"just init\")"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (mod (car modules)))
      (should (= 1 (length modules)))
      (should (equal "minimal-feature" (module-system-module-slug mod)))
      (should-not (module-system-module-spec-file mod))
      (should-not (module-system-module-packages-file mod))
      (should (module-system-module-init-file mod))
      (should (null (module-system-module-lib-files mod)))
      (should-not (module-system-module-tests-file mod)))))

(ert-deftest module-system--discover-lib-files--lib-el-only ()
  "Discovers lib.el when present."
  (module-system-test-with-temp-features
      '(("with-lib" .
         (("lib.el" . "(defun my-func () nil)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (mod (car modules)))
      (should (= 1 (length (module-system-module-lib-files mod))))
      (should (string-suffix-p "lib.el"
                               (car (module-system-module-lib-files mod)))))))

(ert-deftest module-system--discover-lib-files--lib-directory ()
  "Discovers lib/*.el files when lib/ directory exists."
  (module-system-test-with-temp-features
      '(("with-lib-dir" .
         (("lib/utils.el" . "(defun utils-func () nil)")
          ("lib/helpers.el" . "(defun helpers-func () nil)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (mod (car modules)))
      (should (= 2 (length (module-system-module-lib-files mod)))))))

(ert-deftest module-system--discover-lib-files--both-lib-forms ()
  "Discovers both lib.el and lib/*.el files."
  (module-system-test-with-temp-features
      '(("with-both" .
         (("lib.el" . "(defun main-func () nil)")
          ("lib/extra.el" . "(defun extra-func () nil)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (mod (car modules)))
      (should (= 2 (length (module-system-module-lib-files mod)))))))


;;; Package Reading Tests

(ert-deftest module-system--read-packages-file--valid-format ()
  "Reads packages.eld with valid elpaca specs."
  (module-system-test-with-temp-features
      '(("pkg-feature" .
         (("packages.eld" . ";; -*- lisp-data -*-\n((evil :host github :repo \"emacs-evil/evil\")\n (evil-collection))"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (packages (module-system-collect-packages modules)))
      (should (= 2 (length packages)))
      (should (equal '(evil :host github :repo "emacs-evil/evil")
                     (car packages)))
      (should (equal '(evil-collection)
                     (cadr packages))))))

(ert-deftest module-system--read-packages-file--empty-list ()
  "Handles empty packages.eld."
  (module-system-test-with-temp-features
      '(("empty-pkg" .
         (("packages.eld" . "()"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (packages (module-system-collect-packages modules)))
      (should (null packages)))))

(ert-deftest module-system--collect-packages--multiple-modules ()
  "Collects packages from multiple modules."
  (module-system-test-with-temp-features
      '(("feature-a" .
         (("packages.eld" . "((pkg-a) (pkg-b))")))
        ("feature-b" .
         (("packages.eld" . "((pkg-c))"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (packages (module-system-collect-packages modules)))
      (should (= 3 (length packages))))))


;;; Autoload Extraction Tests

(ert-deftest module-system--extract-autoloads--defun ()
  "Extracts autoloads for defun forms."
  (module-system-test-with-temp-features
      '(("autoload-test" .
         (("lib.el" . ";;;###autoload\n(defun my-func (arg)\n  \"Docstring.\"\n  (message arg))"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (autoloads (module-system-collect-autoloads modules)))
      (should (= 1 (length autoloads)))
      (let ((form (caar autoloads)))
        (should (eq 'defun (car form)))
        (should (eq 'my-func (cadr form)))))))

(ert-deftest module-system--extract-autoloads--multiple ()
  "Extracts multiple autoloads from one file."
  (module-system-test-with-temp-features
      '(("multi-autoload" .
         (("lib.el" . ";;;###autoload\n(defun func-1 () nil)\n\n;;;###autoload\n(defun func-2 () nil)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (autoloads (module-system-collect-autoloads modules)))
      (should (= 2 (length autoloads))))))

(ert-deftest module-system--extract-autoloads--ignores-non-autoload ()
  "Only extracts forms preceded by ;;;###autoload."
  (module-system-test-with-temp-features
      '(("mixed" .
         (("lib.el" . "(defun not-autoloaded () nil)\n\n;;;###autoload\n(defun is-autoloaded () nil)"))))
    (let* ((modules (module-system-discover-all temp-features-dir))
           (autoloads (module-system-collect-autoloads modules)))
      (should (= 1 (length autoloads)))
      (should (eq 'is-autoloaded (cadr (caar autoloads)))))))


;;; Autoload Form Generation Tests

(ert-deftest module-system--autoload-form--defun-with-docstring ()
  "Generates correct autoload for defun with docstring."
  (let ((form '(defun my-command (arg) "Do something with ARG." (interactive "s") arg))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-command "/path/to/lib.el" "Do something with ARG." t)
                   (module-system--autoload-form-for-defun form source)))))

(ert-deftest module-system--autoload-form--defun-without-docstring ()
  "Generates correct autoload for defun without docstring."
  (let ((form '(defun my-command (arg) (interactive "s") arg))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-command "/path/to/lib.el" nil t)
                   (module-system--autoload-form-for-defun form source)))))

(ert-deftest module-system--autoload-form--cl-defun ()
  "Generates correct autoload for cl-defun."
  (let ((form '(cl-defun my-cl-command (&key arg) "Doc." arg))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-cl-command "/path/to/lib.el" "Doc." t)
                   (module-system--autoload-form-for-defun form source)))))

(ert-deftest module-system--autoload-form--defmacro ()
  "Generates correct autoload for defmacro."
  (let ((form '(defmacro my-macro (arg) "Macro doc." `(foo ,arg)))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-macro "/path/to/lib.el" "Macro doc." nil 'macro)
                   (module-system--autoload-form-for-defun form source)))))

(ert-deftest module-system--autoload-form--define-minor-mode ()
  "Generates correct autoload for define-minor-mode."
  (let ((form '(define-minor-mode my-mode "Toggle my mode." :lighter " My"))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-mode "/path/to/lib.el" "Toggle my mode." t)
                   (module-system--autoload-form-for-defun form source)))))

(ert-deftest module-system--autoload-form--define-derived-mode ()
  "Generates correct autoload for define-derived-mode."
  (let ((form '(define-derived-mode my-derived-mode prog-mode "MyDerived"
                 "Major mode doc."))
        (source "/path/to/lib.el"))
    (should (equal '(autoload 'my-derived-mode "/path/to/lib.el" "Major mode doc." t)
                   (module-system--autoload-form-for-defun form source)))))


;;; Integration Tests

(ert-deftest module-system--list-modules ()
  "module-system-list-modules returns slug list."
  (module-system-test-with-temp-features
      '(("alpha" . (("init.el" . "")))
        ("beta" . (("init.el" . ""))))
    (let ((slugs (module-system-list-modules temp-features-dir)))
      (should (= 2 (length slugs)))
      (should (member "alpha" slugs))
      (should (member "beta" slugs)))))


(provide 'module-system-tests)
;;; module-system-tests.el ends here
