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

(provide '+modules-tests)

;;; +modules-tests.el ends here
