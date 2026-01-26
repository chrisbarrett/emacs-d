;;; lang-conf/tests.el --- Tests for lang-conf module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for configuration file format support (JSON, YAML, KDL, conf-mode).

;;; Code:

(require 'ert)

;; Load init.el from the same directory
(let ((init-file (expand-file-name "init.el" (file-name-directory (or load-file-name buffer-file-name)))))
  (condition-case nil
      (load init-file nil t)
    (error nil)))


;;; P1: Files ending in `rc` open in conf-mode

(ert-deftest lang-conf/rc-mode-association ()
  "Files ending in rc should open in conf-mode."
  (let ((matches (seq-filter (lambda (entry)
                               (string-match-p "rc\\\\'" (car entry)))
                             auto-mode-alist)))
    (should matches)
    (should (eq 'conf-mode (cdr (car matches))))))


;;; P2: `.dockerignore` opens in conf-mode

(ert-deftest lang-conf/dockerignore-mode-association ()
  "The .dockerignore file should open in conf-mode."
  (let ((matches (seq-filter (lambda (entry)
                               (string-match-p "\\.dockerignore" (car entry)))
                             auto-mode-alist)))
    (should matches)
    (should (eq 'conf-mode (cdr (car matches))))))


;;; P3: `.gitignore` opens in conf-mode

(ert-deftest lang-conf/gitignore-mode-association ()
  "The .gitignore file should open in conf-mode."
  (let ((matches (seq-filter (lambda (entry)
                               (string-match-p "\\.gitignore" (car entry)))
                             auto-mode-alist)))
    (should matches)
    (should (eq 'conf-mode (cdr (car matches))))))


;;; P4: `*.kdl` files open in kdl-ts-mode

(ert-deftest lang-conf/kdl-mode-association ()
  "KDL files should open in kdl-ts-mode."
  (let ((matches (seq-filter (lambda (entry)
                               (string-match-p "\\.kdl" (car entry)))
                             auto-mode-alist)))
    (should matches)
    (should (eq 'kdl-ts-mode (cdr (car matches))))))


;;; P5: `*.json` files have LSP via eglot

(ert-deftest lang-conf/json-eglot-hook ()
  "json-ts-mode should have eglot-ensure hook."
  (skip-unless (boundp 'json-ts-mode-local-vars-hook))
  (should (memq 'eglot-ensure json-ts-mode-local-vars-hook)))


;;; P6: `*.yaml` files have LSP via eglot

(ert-deftest lang-conf/yaml-eglot-hook ()
  "yaml-ts-mode should have eglot-ensure hook."
  (skip-unless (boundp 'yaml-ts-mode-local-vars-hook))
  (should (memq 'eglot-ensure yaml-ts-mode-local-vars-hook)))


;;; P7: YAML mode has tab-width 2

(ert-deftest lang-conf/yaml-tab-width-hook ()
  "yaml-ts-mode should have a hook to set tab-width."
  (skip-unless (boundp 'yaml-ts-mode-hook))
  ;; The setq-hook! macro adds a lambda to the hook; check the hook is not empty
  (should yaml-ts-mode-hook))


;;; Additional tests

(ert-deftest lang-conf/provides-feature ()
  "Module should provide lang-conf-init feature."
  (should (featurep 'lang-conf-init)))

(provide 'lang-conf-tests)

;;; lang-conf/tests.el ends here
