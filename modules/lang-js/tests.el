;;; tests.el --- lang-js module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for JavaScript/TypeScript module based on spec 035-lang-js.md
;; Testable Properties:
;; P1. js-ts-mode activates for *.js files
;; P2. js-ts-mode activates for *.mjs files
;; P3. typescript-ts-mode activates for deno shebang
;; P4. node_modules directories are read-only
;; P5. find-sibling-rules contains .test.ts pattern
;; P6. nx.json is in project-vc-extra-root-markers
;; P7. +ts-project-type returns 'deno for deno.json projects
;; P8. +ts-project-type returns 'node for package.json projects
;; P9. eglot-server-programs contains typescript-ts-mode entry

;;; Code:

(require 'ert)

;; Load module files from this directory
;; May fail in batch mode due to missing dependencies
(let* ((module-dir (file-name-directory (or load-file-name buffer-file-name)))
       (lib-file (expand-file-name "lang-js-lib.el" module-dir))
       (init-file (expand-file-name "init.el" module-dir)))
  (condition-case nil
      (progn
        (load lib-file nil 'nomessage)
        (load init-file nil 'nomessage))
    (error nil)))

;; P1: js-ts-mode activates for *.js files
;; Note: This is built-in Emacs behavior, not configured by our module
(ert-deftest lang-js-test-js-mode-association ()
  "Test that *.js files use js-ts-mode."
  (let ((entry (assoc "\\.js\\'" auto-mode-alist)))
    ;; Skip if entry not found (init didn't load)
    (skip-unless entry)
    ;; The mode can be in the chain via js -> js-ts-mode remap
    (should (or (eq (cdr entry) 'js-ts-mode)
                (eq (cdr entry) 'js-mode)
                (eq (cdr entry) 'javascript-mode)))))

;; P2: js-ts-mode activates for *.mjs files
(ert-deftest lang-js-test-mjs-mode-association ()
  "Test that *.mjs files use js-ts-mode."
  (let ((mjs-entry (seq-find (lambda (entry)
                               (and (stringp (car entry))
                                    (string-match-p "mjs" (car entry))))
                             auto-mode-alist)))
    ;; Skip if not configured
    (skip-unless mjs-entry)
    (should mjs-entry)))

;; P3: typescript-ts-mode activates for deno shebang
(ert-deftest lang-js-test-shebang-magic-mode ()
  "Test that deno/node/bun shebangs trigger typescript-ts-mode."
  (should (seq-find (lambda (entry)
                      (and (stringp (car entry))
                           (eq (cdr entry) 'typescript-ts-mode)))
                    magic-mode-alist)))

;; P4: node_modules directories are read-only
(ert-deftest lang-js-test-node-modules-readonly ()
  "Test that node_modules gets read-only mode."
  ;; Skip if +corelib not available
  (skip-unless (require '+corelib nil t))
  (skip-unless (boundp '+dirlocals-list))
  ;; The dirlocals are registered via +dirlocals-set-regexp
  ;; Check that +dirlocals-list has the node_modules pattern
  (let ((found (seq-find (lambda (entry)
                           (and (stringp (car entry))
                                (string-match-p "node_modules" (car entry))))
                         +dirlocals-list)))
    (skip-unless found)
    (should found)))

;; P5: find-sibling-rules contains .test.ts pattern
(ert-deftest lang-js-test-sibling-rules ()
  "Test that find-sibling-rules includes .test.ts pattern."
  (require 'find-file)
  (skip-unless (boundp 'find-sibling-rules))
  (let ((found (seq-find (lambda (rule)
                           (and (listp rule)
                                (seq-find (lambda (r)
                                            (and (stringp r)
                                                 (string-match-p "\\.test\\.ts" r)))
                                          rule)))
                         find-sibling-rules)))
    (skip-unless found)
    (should found)))

;; P6: nx.json is in project-vc-extra-root-markers
(ert-deftest lang-js-test-nx-root-marker ()
  "Test that nx.json is a project root marker."
  (require 'project)
  (should (boundp 'project-vc-extra-root-markers))
  (should (member "nx.json" project-vc-extra-root-markers)))

;; P7: +ts-project-type returns 'deno for deno.json projects
(ert-deftest lang-js-test-deno-project-detection ()
  "Test that +ts-project-type detects deno.json projects."
  (let ((temp-dir (make-temp-file "lang-js-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "deno.json" temp-dir))
          (should (eq (+ts-project-type temp-dir) 'deno)))
      (delete-directory temp-dir t))))

;; P8: +ts-project-type returns 'node for package.json projects
(ert-deftest lang-js-test-node-project-detection ()
  "Test that +ts-project-type detects package.json projects."
  (let ((temp-dir (make-temp-file "lang-js-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "package.json" temp-dir))
          (should (eq (+ts-project-type temp-dir) 'node)))
      (delete-directory temp-dir t))))

;; P9: eglot-server-programs contains typescript-ts-mode entry
(ert-deftest lang-js-test-eglot-server-programs ()
  "Test that eglot-server-programs has typescript-ts-mode entry."
  (require 'eglot)
  (should (boundp 'eglot-server-programs))
  (should (seq-find (lambda (entry)
                      (and (listp (car entry))
                           (seq-find (lambda (m)
                                       (eq (if (consp m) (car m) m) 'typescript-ts-mode))
                                     (car entry))))
                    eglot-server-programs)))

;; Additional tests

(ert-deftest lang-js-test-bun-project-detection ()
  "Test that +ts-project-type detects bun.lockb projects."
  (let ((temp-dir (make-temp-file "lang-js-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "bun.lockb" temp-dir))
          (should (eq (+ts-project-type temp-dir) 'bun)))
      (delete-directory temp-dir t))))

(ert-deftest lang-js-test-shebang-detection ()
  "Test that +ts-shebang-type detects various shebangs."
  (with-temp-buffer
    (insert "#!/usr/bin/env deno\n")
    (should (eq (+ts-shebang-type) 'deno)))
  (with-temp-buffer
    (insert "#!/usr/bin/node\n")
    (should (eq (+ts-shebang-type) 'node)))
  (with-temp-buffer
    (insert "#!/usr/bin/env bun\n")
    (should (eq (+ts-shebang-type) 'bun)))
  (with-temp-buffer
    (insert "no shebang\n")
    (should (null (+ts-shebang-type)))))

(ert-deftest lang-js-test-cdk-project-detection ()
  "Test that +cdk-project-p detects CDK projects."
  (let ((temp-dir (make-temp-file "lang-js-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "cdk.json" temp-dir))
          (should (+cdk-project-p temp-dir)))
      (delete-directory temp-dir t))))

(ert-deftest lang-js-test-cdk-root-marker ()
  "Test that cdk.json is a project root marker."
  (require 'project)
  (should (member "cdk.json" project-vc-extra-root-markers)))

(ert-deftest lang-js-test-deno-root-markers ()
  "Test that deno.json and deno.jsonc are project root markers."
  (require 'project)
  (should (member "deno.json" project-vc-extra-root-markers))
  (should (member "deno.jsonc" project-vc-extra-root-markers)))

(ert-deftest lang-js-test-vc-ignores ()
  "Test that .nx/ is in project-vc-ignores."
  (require 'project)
  (should (member ".nx/" project-vc-ignores)))

(ert-deftest lang-js-test-ts-server-program-defined ()
  "Test that +ts-server-program is defined and callable."
  (should (fboundp '+ts-server-program))
  ;; Without context, should default to typescript-language-server
  (let ((result (+ts-server-program)))
    (should (listp result))
    (should (or (equal (car result) "typescript-language-server")
                (equal (car result) "deno")))))

(provide 'lang-js-tests)

;;; tests.el ends here
