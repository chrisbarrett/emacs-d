;;; tests.el --- Tests for spellcheck module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for spellcheck configuration based on spec properties.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the module files from this directory
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lib-file (expand-file-name "lib.el" module-dir))
       (init-file (expand-file-name "init.el" module-dir)))
  ;; org-directory is used by init.el for personal dictionary path
  (unless (boundp 'org-directory)
    (defvar org-directory "/tmp/org-test"))
  (load lib-file nil 'nomessage)
  (load init-file nil 'nomessage))

;;; P3-P5: spell-fu-mode hooks

(ert-deftest spellcheck-test-text-mode-hook ()
  "spell-fu-mode should be in text-mode-hook."
  (should (memq #'spell-fu-mode text-mode-hook)))

(ert-deftest spellcheck-test-prog-mode-hook ()
  "spell-fu-mode should be in prog-mode-hook."
  (should (memq #'spell-fu-mode prog-mode-hook)))

;;; P6-P8: Keybindings (require evil)

(ert-deftest spellcheck-test-zn-bound ()
  "zn should be bound to spell-fu-goto-next-error in normal state."
  (skip-unless (featurep 'evil))
  (require 'spell-fu)
  (should (eq (evil-lookup-key evil-normal-state-map "zn")
              #'spell-fu-goto-next-error)))

(ert-deftest spellcheck-test-zg-bound ()
  "zg should be bound to spell-fu-word-add in normal state."
  (skip-unless (featurep 'evil))
  (require 'spell-fu)
  (should (eq (evil-lookup-key evil-normal-state-map "zg")
              #'spell-fu-word-add)))

(ert-deftest spellcheck-test-z-space-bound ()
  "z SPC should be bound to flyspell-correct-at-point in normal state."
  (skip-unless (featurep 'evil))
  (skip-unless (featurep 'flyspell-correct))
  (should (eq (evil-lookup-key evil-normal-state-map (kbd "z SPC"))
              #'flyspell-correct-at-point)))

;;; P9: Org-mode face exclusions

(ert-deftest spellcheck-test-org-excluded-faces-list ()
  "Org-mode excluded faces should include org-link."
  (should (memq 'org-link +spellcheck-org-excluded-faces)))

(ert-deftest spellcheck-test-org-excluded-faces-complete ()
  "Org-mode excluded faces should include all expected faces."
  (let ((expected '(org-meta-line org-link org-code org-block
                    org-block-begin-line org-block-end-line
                    org-footnote org-tag org-modern-tag org-verbatim)))
    (dolist (face expected)
      (should (memq face +spellcheck-org-excluded-faces)))))

;;; Library function tests

(ert-deftest spellcheck-test-dictionaries-list ()
  "Default dictionaries should include en_AU and fr."
  (should (member "en_AU" +spellcheck-dictionaries))
  (should (member "fr" +spellcheck-dictionaries)))

(ert-deftest spellcheck-test-add-dictionaries-callable ()
  "Dictionary add function should be defined."
  (should (fboundp '+spellcheck-add-dictionaries)))

(ert-deftest spellcheck-test-setup-org-callable ()
  "Org setup function should be defined."
  (should (fboundp '+spellcheck-setup-org)))

(provide 'spellcheck-tests)

;;; tests.el ends here
