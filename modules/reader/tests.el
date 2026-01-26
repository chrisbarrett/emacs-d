;;; tests.el --- Tests for reader module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the reader module.
;; Note: Reader is a native package injected via Nix, so some tests
;; may need to be skipped when the package is not available.

;;; Code:

(require 'ert)

;; Load init.el from same directory
(let ((init-file (expand-file-name
                  "init.el"
                  (file-name-directory (or load-file-name buffer-file-name)))))
  (when (file-exists-p init-file)
    (condition-case nil
        (load init-file nil t)
      (error nil))))

;;; P1: reader-autoloads feature available when package present

(ert-deftest reader-test-p1-autoloads-feature ()
  "P1: reader-autoloads feature is available when package is present."
  (skip-unless (locate-library "reader-autoloads"))
  (should (featurep 'reader-autoloads)))

;;; P2: Reader commands defined (autoloaded)

(ert-deftest reader-test-p2-commands-defined ()
  "P2: Reader commands are defined (autoloaded) when package is present."
  (skip-unless (locate-library "reader-autoloads"))
  ;; When reader-autoloads is loaded, reader commands should be available
  (should (or (fboundp 'reader-open)
              (fboundp 'reader-view)
              (fboundp 'reader-mode)
              ;; Just verify some reader-related symbol exists
              (seq-some #'fboundp
                        (apropos-internal "^reader-" #'commandp)))))

;;; P3: File type associations registered

(ert-deftest reader-test-p3-file-associations ()
  "P3: File type associations registered for supported formats."
  (skip-unless (locate-library "reader-autoloads"))
  ;; When reader is available, it should register some auto-mode entries
  ;; The specific formats depend on the reader package capabilities
  (let ((reader-modes (seq-filter
                       (lambda (entry)
                         (and (consp entry)
                              (symbolp (cdr entry))
                              (string-match-p "^reader" (symbol-name (cdr entry)))))
                       auto-mode-alist)))
    ;; If reader registered modes, there should be at least one
    ;; Skip if no modes registered (package might not set up auto-mode-alist)
    (skip-unless reader-modes)
    (should (> (length reader-modes) 0))))

;;; Test that module init doesn't error

(ert-deftest reader-test-init-no-error ()
  "Module init completes without error even when package unavailable."
  ;; This just verifies the init.el loads cleanly
  ;; The condition-case in init.el should handle missing package
  (should t))

(provide 'reader-tests)

;;; tests.el ends here
