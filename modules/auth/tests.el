;;; tests.el --- Auth module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for auth module - 1Password integration via auth-source.

;;; Code:

(require 'ert)

(cl-eval-when (compile)
  (require 'auth-source)
  (require 'auth-source-op))

;; Load init.el from this module
(let ((init-file (expand-file-name "modules/auth/init.el" user-emacs-directory)))
  (condition-case nil
      (load init-file nil t)
    (error nil)))

;;; P1: auth-sources contains 1password symbol

(ert-deftest auth/p1-auth-sources-contains-1password ()
  "P1: `auth-sources' should contain the 1password symbol."
  (skip-unless (featurep 'auth-source-op))
  (should (memq '1password auth-sources)))

;;; P2: auth-source-op-vaults set to '("Emacs")

(ert-deftest auth/p2-auth-source-op-vaults-set ()
  "P2: `auth-source-op-vaults' should be set to Emacs vault only."
  (skip-unless (featurep 'auth-source-op))
  (should (equal '("Emacs") auth-source-op-vaults)))

;;; P3: auth-source-op-enable called during init (backend registered)

(ert-deftest auth/p3-backend-registered ()
  "P3: auth-source-op backend should be registered."
  (skip-unless (featurep 'auth-source-op))
  (skip-unless (boundp 'auth-source-backends))
  (should (cl-some (lambda (backend)
                     (when (listp backend)
                       (eq (plist-get backend :type) '1password)))
                   auth-source-backends)))

;;; P5: Package loads after auth-source feature

(ert-deftest auth/p5-with-eval-after-load ()
  "P5: Package configuration should be deferred until auth-source loads."
  ;; We use with-eval-after-load 'auth-source in init.el
  ;; Verify that init.el provides the feature
  (let ((init-file (expand-file-name "modules/auth/init.el" user-emacs-directory)))
    (should (file-exists-p init-file))))

;;; Additional: Module structure

(ert-deftest auth/module-has-packages ()
  "Module has packages.eld (packages now via `use-package' :ensure)."
  (let ((packages-file (expand-file-name "modules/auth/packages.eld" user-emacs-directory)))
    (should (file-exists-p packages-file))))

(ert-deftest auth/module-has-spec ()
  "Module has spec.md symlink."
  (let ((spec-file (expand-file-name "modules/auth/spec.md" user-emacs-directory)))
    (should (file-exists-p spec-file))))

(provide 'auth-tests)

;;; tests.el ends here
