;;; +modules-tests.el --- Tests for +modules.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require '+modules)

(ert-deftest modules--discover--returns-empty-when-no-modules-dir ()
  "Discovery returns nil when modules directory doesn't exist."
  (let ((+modules-directory (make-temp-name "/tmp/nonexistent-")))
    (should (null (+modules-discover)))))

(ert-deftest modules--discover--returns-empty-when-no-valid-modules ()
  "Discovery returns nil when no valid module directories exist."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (progn
          ;; Create empty subdirectory
          (make-directory (expand-file-name "empty-module" +modules-directory))
          (should (null (+modules-discover))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-init-el ()
  "Discovery finds module containing init.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "init.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-lib-el ()
  "Discovery finds module containing lib.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "lib.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-packages-eld ()
  "Discovery finds module containing packages.eld."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "packages.eld" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-spec-md ()
  "Discovery finds module containing spec.md."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "spec.md" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-module-with-tests-el ()
  "Discovery finds module containing tests.el."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "my-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "tests.el" module-dir))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--finds-multiple-modules ()
  "Discovery finds all valid module directories."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-a (expand-file-name "module-a" +modules-directory))
              (module-b (expand-file-name "module-b" +modules-directory)))
          (make-directory module-a)
          (make-directory module-b)
          (write-region "" nil (expand-file-name "init.el" module-a))
          (write-region "" nil (expand-file-name "lib.el" module-b))
          (let ((result (+modules-discover)))
            (should (= 2 (length result)))
            (should (member module-a result))
            (should (member module-b result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--ignores-hidden-directories ()
  "Discovery ignores directories starting with dot."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((hidden-dir (expand-file-name ".hidden-module" +modules-directory))
              (visible-dir (expand-file-name "visible-module" +modules-directory)))
          (make-directory hidden-dir)
          (make-directory visible-dir)
          (write-region "" nil (expand-file-name "init.el" hidden-dir))
          (write-region "" nil (expand-file-name "init.el" visible-dir))
          (let ((result (+modules-discover)))
            (should (equal (list visible-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--discover--ignores-regular-files ()
  "Discovery ignores regular files in modules directory."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-dir (expand-file-name "real-module" +modules-directory)))
          (make-directory module-dir)
          (write-region "" nil (expand-file-name "init.el" module-dir))
          ;; Create a regular file that shouldn't be treated as a module
          (write-region "" nil (expand-file-name "not-a-module.el" +modules-directory))
          (let ((result (+modules-discover)))
            (should (equal (list module-dir) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--valid-module-p--returns-nil-for-file ()
  "Validation returns nil for regular files."
  (let ((temp-file (make-temp-file "not-a-module")))
    (unwind-protect
        (should-not (+modules--valid-module-p temp-file))
      (delete-file temp-file))))

(ert-deftest modules--valid-module-p--returns-nil-for-empty-dir ()
  "Validation returns nil for empty directories."
  (let ((temp-dir (make-temp-file "empty-module-" t)))
    (unwind-protect
        (should-not (+modules--valid-module-p temp-dir))
      (delete-directory temp-dir t))))

(ert-deftest modules--valid-module-p--returns-t-for-valid-module ()
  "Validation returns t for directories with recognized files."
  (let ((temp-dir (make-temp-file "valid-module-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "init.el" temp-dir))
          (should (+modules--valid-module-p temp-dir)))
      (delete-directory temp-dir t))))

;;; Tests for +modules-read-packages

(ert-deftest modules--read-packages--returns-nil-when-no-file ()
  "Reading packages returns nil when packages.eld doesn't exist."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (should (null (+modules-read-packages temp-dir)))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--reads-single-package ()
  "Reading packages returns spec for single package."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "((evil-collection))" nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (equal '((evil-collection)) result))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--reads-multiple-packages ()
  "Reading packages returns list of multiple package specs."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "((evil :host github :repo \"emacs-evil/evil\")\n (evil-collection))"
                        nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (= 2 (length result)))
            (should (equal '(evil :host github :repo "emacs-evil/evil") (car result)))
            (should (equal '(evil-collection) (cadr result)))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--handles-lisp-data-comment ()
  "Reading packages ignores mode line comment."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region ";; -*- lisp-data -*-\n((consult))" nil packages-file)
          (let ((result (+modules-read-packages temp-dir)))
            (should (equal '((consult)) result))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--returns-nil-for-empty-file ()
  "Reading packages returns nil for empty file."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region "" nil packages-file)
          (should (null (+modules-read-packages temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest modules--read-packages--returns-nil-for-comment-only ()
  "Reading packages returns nil when file contains only comments."
  (let ((temp-dir (make-temp-file "module-" t)))
    (unwind-protect
        (let ((packages-file (expand-file-name "packages.eld" temp-dir)))
          (write-region ";; -*- lisp-data -*-\n;; No packages yet" nil packages-file)
          (should (null (+modules-read-packages temp-dir))))
      (delete-directory temp-dir t))))

;;; Tests for +modules-collect-packages

(ert-deftest modules--collect-packages--returns-empty-when-no-modules ()
  "Collecting packages returns nil when no modules exist."
  (let ((+modules-directory (make-temp-name "/tmp/nonexistent-")))
    (should (null (+modules-collect-packages)))))

(ert-deftest modules--collect-packages--aggregates-from-multiple-modules ()
  "Collecting packages aggregates specs from all modules."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((module-a (expand-file-name "module-a" +modules-directory))
              (module-b (expand-file-name "module-b" +modules-directory)))
          (make-directory module-a)
          (make-directory module-b)
          (write-region "((evil))" nil (expand-file-name "packages.eld" module-a))
          (write-region "((consult) (vertico))" nil (expand-file-name "packages.eld" module-b))
          (let ((result (+modules-collect-packages)))
            (should (= 3 (length result)))
            (should (member '(evil) result))
            (should (member '(consult) result))
            (should (member '(vertico) result))))
      (delete-directory +modules-directory t))))

(ert-deftest modules--collect-packages--skips-modules-without-packages ()
  "Collecting packages skips modules without packages.eld."
  (let ((+modules-directory (make-temp-file "modules-" t)))
    (unwind-protect
        (let ((with-packages (expand-file-name "with-packages" +modules-directory))
              (without-packages (expand-file-name "without-packages" +modules-directory)))
          (make-directory with-packages)
          (make-directory without-packages)
          (write-region "((evil))" nil (expand-file-name "packages.eld" with-packages))
          (write-region "" nil (expand-file-name "init.el" without-packages))
          (let ((result (+modules-collect-packages)))
            (should (equal '((evil)) result))))
      (delete-directory +modules-directory t))))

(provide '+modules-tests)

;;; +modules-tests.el ends here
