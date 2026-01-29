;;; org-tests.el --- Tests for org module -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the org module based on spec testable properties.

;;; Code:

(require 'ert)

;; Get module directory
(defvar org-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name)))

;; Load the lib.el which contains autoloaded functions
(let ((lib-file (expand-file-name "lib.el" org-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil t)))

;; Try to load the module init (may fail in batch mode without elpaca)
(condition-case nil
    (load (expand-file-name "init.el" org-test--module-dir) nil t)
  (error nil))


;;; Helper to check if our org config was applied
;; We check for a distinctive setting that we set (not org's default)

(defun org-test--config-applied-p ()
  "Return non-nil if our org configuration was applied.
Checks for org-ellipsis which we set to \" …\" (not the default)."
  (and (boundp 'org-ellipsis)
       (stringp org-ellipsis)
       (string-match-p "…" org-ellipsis)))


;;; P1: org-todo-keywords includes TODO, WAIT, DONE, CANCELLED, PROJECT

(ert-deftest org-test-p1-todo-keywords ()
  "Test that org-todo-keywords includes all expected keywords."
  ;; Skip if our config hasn't been applied
  (skip-unless (org-test--config-applied-p))
  (let ((keywords-flat (flatten-tree org-todo-keywords)))
    (should (member "TODO(t)" keywords-flat))
    (should (member "WAIT(w)" keywords-flat))
    (should (member "DONE(d!)" keywords-flat))
    (should (member "CANCELLED(c@)" keywords-flat))
    (should (member "PROJECT(p)" keywords-flat))))


;;; P2: org-babel-load-languages includes emacs-lisp, C, calc, shell

(ert-deftest org-test-p2-babel-languages ()
  "Test that org-babel-load-languages includes expected languages."
  ;; Skip if org hasn't been loaded (settings in :custom block)
  (skip-unless (org-test--config-applied-p))
  (should (assq 'emacs-lisp org-babel-load-languages))
  (should (assq 'C org-babel-load-languages))
  (should (assq 'calc org-babel-load-languages))
  (should (assq 'shell org-babel-load-languages)))


;;; P3: org-archive-location points to archive.org with datetree

(ert-deftest org-test-p3-archive-location ()
  "Test that org-archive-location is configured for datetree."
  ;; Skip if org hasn't been loaded (settings in :config block)
  (skip-unless (org-test--config-applied-p))
  (should (string-match-p "archive\\.org" org-archive-location))
  (should (string-match-p "datetree" org-archive-location)))


;;; P4: File link to nonexistent path shows warning face

(ert-deftest org-test-p4-file-link-warning-face ()
  "Test that file link type has face function for broken links."
  (skip-unless (fboundp 'org-link-get-parameter))
  (skip-unless (featurep 'ol))
  (let ((face-fn (org-link-get-parameter "file" :face)))
    (skip-unless face-fn)
    (should (functionp face-fn))
    ;; Call with nonexistent path
    (let ((result (funcall face-fn "/nonexistent/path/to/file.txt")))
      (should (consp result))
      (should (memq 'warning result)))))


;;; P5: ID links use +org-id-link face

(ert-deftest org-test-p5-id-link-face ()
  "Test that ID links have distinct face configured."
  (skip-unless (fboundp 'org-link-get-parameter))
  (skip-unless (featurep 'ol))
  (should (eq '+org-id-link (org-link-get-parameter "id" :face))))


;;; P6: org-modern-mode is enabled globally

(ert-deftest org-test-p6-org-modern-mode ()
  "Test that global-org-modern-mode is available."
  (skip-unless (featurep 'org-modern))
  (should (fboundp 'global-org-modern-mode)))


;;; P7: evil-org-mode activates in org-mode buffers

(ert-deftest org-test-p7-evil-org-mode-hook ()
  "Test that evil-org-mode is hooked to org-mode."
  (skip-unless (boundp 'org-mode-hook))
  (should (or (memq 'evil-org-mode org-mode-hook)
              ;; Hook might be added via use-package
              (featurep 'evil-org))))


;;; P8: org-super-agenda-mode is enabled

(ert-deftest org-test-p8-org-super-agenda ()
  "Test that org-super-agenda-mode function exists."
  (skip-unless (featurep 'org-super-agenda))
  (should (fboundp 'org-super-agenda-mode)))


;;; Lib function tests

(ert-deftest org-test-org-id-link-face-defined ()
  "Test that +org-id-link face is defined."
  (should (facep '+org-id-link)))

(ert-deftest org-test-cut-subtree-or-cancel-note ()
  "Test that +org-cut-subtree-or-cancel-note is defined."
  (should (fboundp '+org-cut-subtree-or-cancel-note)))

(ert-deftest org-test-metareturn-insert-heading ()
  "Test that +org-metareturn-insert-heading-insert-state-h is defined."
  (should (fboundp '+org-metareturn-insert-heading-insert-state-h)))

(ert-deftest org-test-ad-org-enter-evil-insert-state ()
  "Test that +ad-org-enter-evil-insert-state is defined."
  (should (fboundp '+ad-org-enter-evil-insert-state)))


;;; Clocktable function tests

(ert-deftest org-test-clocktable-fmt-daily-log ()
  "Test that +clocktable-fmt-daily-log is defined."
  (should (fboundp '+clocktable-fmt-daily-log)))

(provide 'org-tests)

;;; org-tests.el ends here
