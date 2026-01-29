;;; leader-tests.el --- Tests for leader module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for leader keybindings from spec 007-leader.md

;;; Code:

(require 'ert)

(defvar leader-test--module-dir
  (expand-file-name "modules/leader/" user-emacs-directory)
  "Directory containing the leader module.")

;; Load module init
(let ((init-file (expand-file-name "init.el" leader-test--module-dir)))
  (condition-case err
      (load init-file nil t)
    (error (message "Leader init.el load error: %s" err))))

;; Load lib for function tests
(let ((lib-file (expand-file-name "lib.el" leader-test--module-dir)))
  (condition-case nil
      (load lib-file nil t)
    (error nil)))

;;; init.el content verification for batch mode

(ert-deftest leader-test-init-contains-leader-key-def ()
  "init.el contains +leader-key prefix command definition."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":prefix-command '+leader-key" nil t)))))

(ert-deftest leader-test-init-contains-m-m-binding ()
  "init.el contains M-m global binding."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "(general-def \"M-m\" '+leader-key)" nil t)))))

(ert-deftest leader-test-init-contains-universal-arg-chaining ()
  "init.el contains universal-argument chaining setup."
  (let ((file (expand-file-name "init.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "universal-argument-map" nil t)))))

;;; lib.el content verification

(ert-deftest leader-test-lib-contains-autoloads ()
  "lib.el contains autoload cookies for public functions."
  (let ((file (expand-file-name "lib.el" leader-test--module-dir)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ";;;###autoload" nil t)))))

(provide 'leader-tests)

;;; leader-tests.el ends here
