;;; tests.el --- Auth module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for auth module - 1Password integration via auth-source.

;;; Code:

(require 'ert)

(cl-eval-when (compile)
  (require 'auth-source)
  (require 'auth-source-op))

;; Load init.el from this module
(load (expand-file-name "modules/auth/init.el" user-emacs-directory) nil t)

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

;;; tests.el ends here
