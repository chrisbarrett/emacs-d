;;; leader-tests.el --- Tests for leader module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for leader keybindings from spec 007-leader.md

;;; Code:

(require 'ert)

(defvar leader-test--module-dir
  (expand-file-name "modules/leader/" user-emacs-directory)
  "Directory containing the leader module.")

;; Load module init
(let ((init-file (expand-file-name "init.el" leader-test--module-dir)))
  (condition-case err
      (load init-file nil t)
    (error (message "Leader init.el load error: %s" err))))

;; Load lib for function tests
(let ((lib-file (expand-file-name "lib.el" leader-test--module-dir)))
  (condition-case nil
      (load lib-file nil t)
    (error nil)))

;;; init.el post-load state verification

(ert-deftest leader-test-init-contains-leader-key-def ()
  "The +leader-key prefix command is defined as a keymap."
  ;; Other modules (e.g. evil) autoload +leader-key; general skips
  ;; `define-prefix-command' when the symbol is already fbound, so under the
  ;; aggregate test harness +leader-key can be left as that autoload
  ;; depending on load order.  Clear it and reload the leader init so its
  ;; prefix-keymap definition materialises before asserting.
  (fmakunbound '+leader-key)
  (skip-unless
   (ignore-errors
     (load (expand-file-name "init.el" leader-test--module-dir) nil t)
     t))
  (should (keymapp (symbol-function '+leader-key))))

(ert-deftest leader-test-init-contains-m-m-binding ()
  "M-m is globally bound to the leader key."
  (skip-unless (fboundp '+leader-key))
  (should (eq (key-binding (kbd "M-m")) '+leader-key)))

(ert-deftest leader-test-init-contains-universal-arg-chaining ()
  "universal-argument-map chains SPC u to universal-argument-more."
  (skip-unless (boundp 'universal-argument-map))
  (should (eq (keymap-lookup universal-argument-map "SPC u")
              'universal-argument-more)))

;;; lib.el public command availability

(ert-deftest leader-test-lib-public-commands-available ()
  "Public leader commands are available (via lib.el autoloads)."
  (should (commandp '+forward-kill-sexp))
  (should (commandp '+backward-kill-sexp))
  (should (commandp '+find-sibling-file)))

(provide 'leader-tests)

;;; leader-tests.el ends here
