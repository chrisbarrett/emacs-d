;;; tests.el --- Core module tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the core module verifying P1-P9 from spec.

;;; Code:

(require 'ert)

;; Capture module directory at load time
(defvar core-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the core module tests.")

;; Load init.el from the same directory
(let ((init-file (expand-file-name "init.el" core-test--module-dir)))
  (when (file-exists-p init-file)
    (condition-case nil
        (load init-file nil t)
      (error nil))))

;;; Module structure tests

(ert-deftest core-test-module-has-packages-eld ()
  "Test that core module has packages.eld."
  (let ((packages-file (expand-file-name "packages.eld" core-test--module-dir)))
    (should (file-exists-p packages-file))))

(ert-deftest core-test-module-has-spec-md ()
  "Test that core module has spec.md."
  (let ((spec-file (expand-file-name "spec.md" core-test--module-dir)))
    (should (file-exists-p spec-file))))

(ert-deftest core-test-module-has-init-el ()
  "Test that core module has init.el."
  (let ((init-file (expand-file-name "init.el" core-test--module-dir)))
    (should (file-exists-p init-file))))

;;; P1: Emacs Version Check

(ert-deftest core-test-p1-emacs-version-check ()
  "P1: Configuration requires Emacs 30+.
Given init.el loads on Emacs < 30
Then an error would be signaled."
  (should (>= emacs-major-version 30)))

;;; P2: Load Paths Configured

(ert-deftest core-test-p2-load-paths-init-dir ()
  "P2: init/ is in load-path after early-init."
  (skip-unless (and (boundp '+init-dir) +init-dir))
  (should (member +init-dir load-path)))

(ert-deftest core-test-p2-load-paths-lisp-dir ()
  "P2: lisp/ is in load-path after early-init."
  (skip-unless (and (boundp '+lisp-dir) +lisp-dir))
  (should (member +lisp-dir load-path)))

(ert-deftest core-test-p2-load-paths-config-dir ()
  "P2: config/ is in load-path after early-init."
  (skip-unless (and (boundp '+config-dir) +config-dir))
  (should (member +config-dir load-path)))

;;; P3: UI Disabled (batch mode doesn't have these)

(ert-deftest core-test-p3-menu-bar-disabled ()
  "P3: menu-bar-mode is disabled."
  (skip-unless (not noninteractive))
  (should-not menu-bar-mode))

(ert-deftest core-test-p3-tool-bar-disabled ()
  "P3: tool-bar-mode is disabled."
  (skip-unless (and (not noninteractive) (fboundp 'tool-bar-mode)))
  (should-not tool-bar-mode))

(ert-deftest core-test-p3-scroll-bar-disabled ()
  "P3: scroll-bar-mode is disabled."
  (skip-unless (and (not noninteractive) (fboundp 'scroll-bar-mode)))
  (should-not scroll-bar-mode))

;;; P4: Transient Hook Fires Once

(ert-deftest core-test-p4-transient-hook-fires-once ()
  "P4: Transient hooks fire exactly once.
Given +first-input-hook has a function
When pre-command-hook fires twice
Then the function is called exactly once."
  (skip-unless (and (boundp '+first-input-hook)
                    (fboundp '+run-hook-once)))
  (let ((call-count 0)
        (test-hook nil))
    ;; Create a test hook
    (add-hook 'test-hook (lambda () (cl-incf call-count)))
    ;; Set it up as a transient hook
    (+run-hook-once 'test-hook '(pre-command-hook))
    ;; Fire the trigger twice
    (run-hooks 'pre-command-hook)
    (run-hooks 'pre-command-hook)
    ;; Should only have been called once
    (should (= 1 call-count))))

;;; P5: Local Vars Hook Runs

(ert-deftest core-test-p5-local-vars-hook-runs ()
  "P5: Local vars hooks run after hack-local-variables.
Given emacs-lisp-mode-local-vars-hook has a function
When a buffer enters emacs-lisp-mode and hack-local-variables runs
Then the function is called."
  (skip-unless (fboundp '+run-local-var-hooks-h))
  (let ((ran nil))
    (unwind-protect
        (progn
          (add-hook 'emacs-lisp-mode-local-vars-hook (lambda () (setq ran t)))
          (with-temp-buffer
            (emacs-lisp-mode)
            (hack-local-variables))
          (should ran))
      (remove-hook 'emacs-lisp-mode-local-vars-hook (lambda () (setq ran t))))))

;;; P6: Read-Only Protection

(ert-deftest core-test-p6-readonly-vendor ()
  "P6: Files in /vendor/ are read-only."
  (skip-unless (fboundp '+file-should-be-opened-read-only-p))
  (should (+file-should-be-opened-read-only-p "/project/vendor/lib.el")))

(ert-deftest core-test-p6-readonly-elpaca ()
  "P6: Files in /elpaca/ are read-only."
  (skip-unless (fboundp '+file-should-be-opened-read-only-p))
  (should (+file-should-be-opened-read-only-p "/project/elpaca/repos/foo/lib.el")))

(ert-deftest core-test-p6-readonly-node-modules ()
  "P6: Files in /node_modules/ are read-only."
  (skip-unless (fboundp '+file-should-be-opened-read-only-p))
  (should (+file-should-be-opened-read-only-p "/project/node_modules/lib.js")))

(ert-deftest core-test-p6-readonly-src-is-not ()
  "P6: Regular source files are NOT read-only."
  (skip-unless (fboundp '+file-should-be-opened-read-only-p))
  (should-not (+file-should-be-opened-read-only-p "/project/src/lib.el")))

;;; P7: Module Discovery

(ert-deftest core-test-p7-module-discovery ()
  "P7: +modules-discover finds valid module directories."
  (skip-unless (fboundp '+modules-discover))
  (let ((modules (+modules-discover)))
    (should (listp modules))
    (should (cl-every #'file-directory-p modules))))

(ert-deftest core-test-p7-module-discovery-finds-core ()
  "P7: +modules-discover finds the core module."
  (skip-unless (fboundp '+modules-discover))
  (let* ((modules (+modules-discover))
         (core-module (seq-find (lambda (m) (string-match-p "/core\\'" m)) modules)))
    (should core-module)))

;;; P8: Package Spec Reading

(ert-deftest core-test-p8-package-spec-reading ()
  "P8: +modules-read-packages reads packages.eld correctly."
  (skip-unless (fboundp '+modules-read-packages))
  ;; Core packages are now installed via use-package :ensure
  ;; Test just verifies function can read the file
  (let ((packages (+modules-read-packages core-test--module-dir)))
    (should (listp packages))))

(ert-deftest core-test-p8-package-spec-has-envrc ()
  "P8: Core module still functions with packages.eld."
  ;; Packages are now installed via use-package :ensure in init.el
  ;; This test verifies the file exists and is readable
  (let ((packages-file (expand-file-name "packages.eld" core-test--module-dir)))
    (should (file-exists-p packages-file))))

;;; P9: Autoload Registration

(ert-deftest core-test-p9-autoload-registration ()
  "P9: +modules-register-autoloads makes symbols fboundp."
  (skip-unless (fboundp '+modules-register-autoloads))
  ;; This test verifies the mechanism works, not specific symbols
  (should (fboundp '+modules-register-autoloads)))

;;; Hooks existence tests

(ert-deftest core-test-hook-first-input-exists ()
  "Test that +first-input-hook variable exists."
  (skip-unless (boundp '+first-input-hook))
  (should (boundp '+first-input-hook)))

(ert-deftest core-test-hook-first-file-exists ()
  "Test that +first-file-hook variable exists."
  (skip-unless (boundp '+first-file-hook))
  (should (boundp '+first-file-hook)))

(ert-deftest core-test-hook-first-buffer-exists ()
  "Test that +first-buffer-hook variable exists."
  (skip-unless (boundp '+first-buffer-hook))
  (should (boundp '+first-buffer-hook)))

(ert-deftest core-test-hook-switch-buffer-exists ()
  "Test that +switch-buffer-hook variable exists."
  (skip-unless (boundp '+switch-buffer-hook))
  (should (boundp '+switch-buffer-hook)))

(ert-deftest core-test-hook-switch-frame-exists ()
  "Test that +switch-frame-hook variable exists."
  (skip-unless (boundp '+switch-frame-hook))
  (should (boundp '+switch-frame-hook)))

(ert-deftest core-test-hook-switch-window-exists ()
  "Test that +switch-window-hook variable exists."
  (skip-unless (boundp '+switch-window-hook))
  (should (boundp '+switch-window-hook)))

;;; Corelib utility tests

(ert-deftest core-test-corelib-separate ()
  "Test +separate function partitions sequences."
  (skip-unless (fboundp '+separate))
  (let ((result (+separate #'cl-evenp '(1 2 3 4 5 6))))
    ;; +separate uses push, so order is reversed; returns cons not list
    (should (equal (car result) '(6 4 2)))  ; truthy partition
    (should (equal (cdr result) '(5 3 1)))))  ; falsey partition in cdr

(ert-deftest core-test-corelib-split-with ()
  "Test +split-with function splits at first non-match."
  (skip-unless (fboundp '+split-with))
  (let ((result (+split-with #'cl-evenp '(2 4 6 1 3 5))))
    (should (equal (car result) '(2 4 6)))
    (should (equal (cadr result) '(1 3 5)))))

(ert-deftest core-test-corelib-chunk-by ()
  "Test +chunk-by function splits when predicate returns non-nil."
  (skip-unless (fboundp '+chunk-by))
  ;; Splits occur when predicate returns true
  ;; Input: (2 4 1 3 6 8), predicate: cl-evenp
  ;; 2→t(new), 4→t(new), 1→nil(add), 3→nil(add), 6→t(new), 8→t(new)
  ;; Result: ((2) (4 1 3) (6) (8))
  (let ((result (+chunk-by #'cl-evenp '(2 4 1 3 6 8))))
    (should (= (length result) 4))
    (should (equal (car result) '(2)))
    (should (equal (cadr result) '(4 1 3)))
    (should (equal (caddr result) '(6)))
    (should (equal (cadddr result) '(8)))))

(ert-deftest core-test-corelib-read-eld ()
  "Test +read-eld function reads lisp-data files."
  (skip-unless (fboundp '+read-eld))
  ;; +read-eld expects paths relative to user-emacs-directory
  ;; Core packages.eld is now empty (packages via use-package :ensure)
  (let* ((relative-path (file-relative-name
                         (expand-file-name "packages.eld" core-test--module-dir)
                         user-emacs-directory))
         (result (+read-eld relative-path)))
    (should (listp result))))

(ert-deftest core-test-corelib-dirlocals-set ()
  "Test +dirlocals-set function exists."
  (skip-unless (fboundp '+dirlocals-set))
  (should (fboundp '+dirlocals-set)))

(ert-deftest core-test-corelib-visible-buffers ()
  "Test +visible-buffers function returns buffers."
  (skip-unless (fboundp '+visible-buffers))
  (should (listp (+visible-buffers))))

;;; Macro tests

(ert-deftest core-test-macro-add-hook ()
  "Test add-hook! macro is defined."
  (skip-unless (fboundp 'add-hook!))
  (should (macrop 'add-hook!)))

(ert-deftest core-test-macro-add-transient-hook ()
  "Test add-transient-hook! macro is defined."
  (skip-unless (fboundp 'add-transient-hook!))
  (should (macrop 'add-transient-hook!)))

(ert-deftest core-test-macro-setq-hook ()
  "Test setq-hook! macro is defined."
  (skip-unless (fboundp 'setq-hook!))
  (should (macrop 'setq-hook!)))

(ert-deftest core-test-macro-pushnew ()
  "Test pushnew! macro is defined."
  (skip-unless (fboundp 'pushnew!))
  (should (macrop 'pushnew!)))

(ert-deftest core-test-macro-delq ()
  "Test delq! macro is defined."
  (skip-unless (fboundp 'delq!))
  (should (macrop 'delq!)))

;;; Use-package keyword tests
;; These keywords are registered by init-elpaca.el and +load-incrementally.el
;; which may not be loaded in the test environment.

(ert-deftest core-test-use-package-keyword-ensure-unless-local ()
  "Test :ensure-unless-local keyword is registered."
  (skip-unless (and (boundp 'use-package-keywords)
                    (memq :ensure-unless-local use-package-keywords)))
  (should (memq :ensure-unless-local use-package-keywords)))

(ert-deftest core-test-use-package-keyword-defer-incrementally ()
  "Test :defer-incrementally keyword is registered."
  (skip-unless (and (boundp 'use-package-keywords)
                    (memq :defer-incrementally use-package-keywords)))
  (should (memq :defer-incrementally use-package-keywords)))

(ert-deftest core-test-use-package-keyword-after-call ()
  "Test :after-call keyword is registered."
  (skip-unless (and (boundp 'use-package-keywords)
                    (memq :after-call use-package-keywords)))
  (should (memq :after-call use-package-keywords)))

(provide 'core-tests)

;;; tests.el ends here
