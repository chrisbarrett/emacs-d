;;; lang-swift/tests.el --- Tests for lang-swift module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Swift language support module.

;;; Code:

(require 'ert)

(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (load init-file nil 'nomessage)
    (error nil)))

;;; P1: .swift files open in swift-ts-mode

(ert-deftest lang-swift/auto-mode ()
  "P1: .swift files should be associated with swift-ts-mode."
  (let ((entry (assoc "\\.swift\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'swift-ts-mode))))

;;; P1b: .swiftinterface files open in swift-ts-mode

(ert-deftest lang-swift/auto-mode-interface ()
  "P1b: generated .swiftinterface files should open in swift-ts-mode."
  (let ((entry (assoc "\\.swiftinterface\\'" auto-mode-alist)))
    (should entry)
    (should (eq (cdr entry) 'swift-ts-mode))))

;;; P2: swift-ts-mode-local-vars-hook contains eglot-ensure

(ert-deftest lang-swift/eglot-hook ()
  "P2: swift-ts-mode-local-vars-hook should contain eglot-ensure."
  (should (memq 'eglot-ensure swift-ts-mode-local-vars-hook)))

;;; P3: eglot knows how to launch sourcekit-lsp for swift

(ert-deftest lang-swift/eglot-server ()
  "P3: eglot-server-programs should map swift-ts-mode to sourcekit-lsp."
  (require 'eglot)
  (let ((entry (assoc 'swift-ts-mode eglot-server-programs)))
    (should entry)
    (should (member "sourcekit-lsp" (cdr entry)))))

;;; P4: apheleia formats swift buffers with swift-format

(ert-deftest lang-swift/apheleia-formatter ()
  "P4: apheleia should format swift-ts-mode buffers with swift-format."
  (require 'apheleia)
  (should (eq (alist-get 'swift-ts-mode apheleia-mode-alist) 'swift-format))
  (should (assq 'swift-format apheleia-formatters)))

(provide 'lang-swift-tests)

;;; lang-swift/tests.el ends here
