;;; tests.el --- Tests for lang-erlang module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the Erlang/BEAM module configuration.

;;; Code:

(require 'ert)

;; Load init.el from this module
(condition-case nil
    (load (expand-file-name "init.el"
                            (file-name-directory (or load-file-name buffer-file-name))))
  (error nil))

;;; Testable Properties from spec

;; P1: .beam files hidden in completion
(ert-deftest lang-erlang/beam-hidden-in-completion ()
  "BEAM files should be hidden from completion."
  (should (member ".beam" completion-ignored-extensions)))

;; P2: .jam files hidden in completion
(ert-deftest lang-erlang/jam-hidden-in-completion ()
  "JAM files should be hidden from completion."
  (should (member ".jam" completion-ignored-extensions)))

;; P3: .vee files hidden in completion
(ert-deftest lang-erlang/vee-hidden-in-completion ()
  "VEE files should be hidden from completion."
  (should (member ".vee" completion-ignored-extensions)))

;; P4: .beam files omitted in dired-omit-mode
(ert-deftest lang-erlang/beam-omitted-in-dired ()
  "BEAM files should be omitted in dired-omit-mode."
  (skip-unless (featurep 'dired-x))
  (should (member ".beam" dired-omit-extensions)))

;; P5: erlang package not loaded (disabled)
(ert-deftest lang-erlang/erlang-not-loaded ()
  "Erlang package should not be loaded (disabled)."
  (should-not (featurep 'erlang)))

;;; tests.el ends here
