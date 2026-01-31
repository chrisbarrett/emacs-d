;;; tests.el --- Auth module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for auth module - 1Password integration via auth-source.

;;; Code:

(require 'ert)
(require '+corelib)

(cl-eval-when (compile)
  (require 'auth-source)
  (require 'auth-source-op))

(defmacro auth-test-setup (&rest body)
  "Load auth module and execute BODY."
  (declare (indent 0))
  `(progn
     (+load "init.el")
     ,@body))

;;; P1: auth-sources contains 1password symbol

(ert-deftest auth/p1-auth-sources-contains-1password ()
  "P1: `auth-sources' should contain the 1password symbol."
  (auth-test-setup
   (skip-unless (featurep 'auth-source-op))
   (should (memq '1password auth-sources))))

;;; tests.el ends here
