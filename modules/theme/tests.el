;;; tests.el --- Tests for theme module.  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for theme detection, switching, and system integration.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the lib.el from this module
(let ((lib-file (expand-file-name "lib.el"
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))
  (load lib-file nil 'nomessage))

;;; P1: Theme Variables Set

(ert-deftest theme-test-variables-exist ()
  "Theme variables should exist."
  (should (boundp '+theme-light))
  (should (boundp '+theme-dark))
  (should (boundp '+theme-changed-hook))
  (should (boundp '+theme-override)))

;;; P2: System Query Dispatches

(ert-deftest theme-test-system-query-darwin ()
  "System query on darwin returns a string."
  (should (stringp (+system-theme-query 'darwin))))

(ert-deftest theme-test-system-query-gnu-linux ()
  "System query on GNU/Linux returns a string."
  ;; This may error if gsettings isn't available, which is OK for testing
  (condition-case err
      (should (stringp (+system-theme-query 'gnu/linux)))
    (error
     ;; If gsettings is not available, skip the test
     (ert-skip (format "gsettings not available: %s" err)))))

;;; P3: Theme Detection Logic

(ert-deftest theme-test-detection-returns-dark-for-dark-system ()
  "Theme detection returns dark theme when system reports dark."
  (let ((+theme-dark 'test-dark)
        (+theme-light 'test-light)
        (+theme-override nil))
    (cl-letf (((symbol-function '+system-theme-query)
               (lambda (_) "dark")))
      (should (eq 'test-dark (+theme-for-system-theme))))))

(ert-deftest theme-test-detection-returns-light-for-light-system ()
  "Theme detection returns light theme when system reports light."
  (let ((+theme-dark 'test-dark)
        (+theme-light 'test-light)
        (+theme-override nil))
    (cl-letf (((symbol-function '+system-theme-query)
               (lambda (_) "light")))
      (should (eq 'test-light (+theme-for-system-theme))))))

(ert-deftest theme-test-detection-handles-empty-response ()
  "Theme detection returns light theme for empty system response."
  (let ((+theme-dark 'test-dark)
        (+theme-light 'test-light)
        (+theme-override nil))
    (cl-letf (((symbol-function '+system-theme-query)
               (lambda (_) "")))
      (should (eq 'test-light (+theme-for-system-theme))))))

;;; P4: Theme Override Takes Precedence

(ert-deftest theme-test-override-takes-precedence ()
  "Theme override should take precedence over system detection."
  (let ((+theme-dark 'test-dark)
        (+theme-light 'test-light)
        (+theme-override 'custom-theme))
    (should (eq 'custom-theme (+theme-for-system-theme)))))

(ert-deftest theme-test-nil-override-uses-system ()
  "Nil override should fall back to system detection."
  (let ((+theme-dark 'test-dark)
        (+theme-light 'test-light)
        (+theme-override nil))
    (cl-letf (((symbol-function '+system-theme-query)
               (lambda (_) "dark")))
      (should (eq 'test-dark (+theme-for-system-theme))))))

;;; P5: Theme Changed Hook Runs

(ert-deftest theme-test-hook-runs-on-update ()
  "Hook should run when +theme-update is called."
  (let ((ran nil)
        (+theme-dark 'modus-vivendi)
        (+theme-light 'modus-operandi)
        (+theme-override 'modus-operandi)
        (+theme-changed-hook nil))
    (add-hook '+theme-changed-hook (lambda () (setq ran t)))
    (+theme-update)
    (should ran)))

(ert-deftest theme-test-hook-runs-on-dark ()
  "Hook should run when +theme-dark is called."
  (let ((ran nil)
        (+theme-dark 'modus-vivendi)
        (+theme-changed-hook nil))
    (add-hook '+theme-changed-hook (lambda () (setq ran t)))
    (+theme-dark)
    (should ran)))

(ert-deftest theme-test-hook-runs-on-light ()
  "Hook should run when +theme-light is called."
  (let ((ran nil)
        (+theme-light 'modus-operandi)
        (+theme-changed-hook nil))
    (add-hook '+theme-changed-hook (lambda () (setq ran t)))
    (+theme-light)
    (should ran)))

;;; P6 & P7: Dark/Light Theme Detection

(ert-deftest theme-test-dark-detection-with-dark-theme ()
  "Dark detection returns non-nil for dark themes."
  (let ((custom-enabled-themes nil))
    (load-theme 'modus-vivendi t)
    (unwind-protect
        (should (+theme-dark-p))
      (disable-theme 'modus-vivendi))))

(ert-deftest theme-test-dark-detection-with-light-theme ()
  "Dark detection returns nil for light themes."
  (let ((custom-enabled-themes nil))
    (load-theme 'modus-operandi t)
    (unwind-protect
        (should-not (+theme-dark-p))
      (disable-theme 'modus-operandi))))

;;; Theme switching tests

(ert-deftest theme-test-update-disables-previous-themes ()
  "Theme update should disable previous themes."
  (let ((+theme-override 'modus-operandi)
        (+theme-changed-hook nil))
    (load-theme 'modus-vivendi t)
    (+theme-update)
    (should-not (memq 'modus-vivendi custom-enabled-themes))
    (should (memq 'modus-operandi custom-enabled-themes))
    ;; Cleanup
    (disable-theme 'modus-operandi)))

(ert-deftest theme-test-dark-loads-dark-theme ()
  "Calling +theme-dark loads the dark theme."
  (let ((+theme-dark 'modus-vivendi)
        (+theme-changed-hook nil))
    (+theme-dark)
    (should (memq 'modus-vivendi custom-enabled-themes))
    ;; Cleanup
    (disable-theme 'modus-vivendi)))

(ert-deftest theme-test-light-loads-light-theme ()
  "Calling +theme-light loads the light theme."
  (let ((+theme-light 'modus-operandi)
        (+theme-changed-hook nil))
    (+theme-light)
    (should (memq 'modus-operandi custom-enabled-themes))
    ;; Cleanup
    (disable-theme 'modus-operandi)))

;;; Functions are interactive

(ert-deftest theme-test-dark-is-interactive ()
  "+theme-dark should be an interactive command."
  (should (commandp '+theme-dark)))

(ert-deftest theme-test-light-is-interactive ()
  "+theme-light should be an interactive command."
  (should (commandp '+theme-light)))

(provide 'theme-tests)

;;; tests.el ends here
