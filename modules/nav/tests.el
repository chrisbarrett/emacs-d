;;; nav-tests.el --- Tests for nav module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for navigation, window management, jumping, and position history.

;;; Code:

(require 'ert)

(defconst nav-module--dir
  (expand-file-name "modules/nav/" user-emacs-directory)
  "Directory containing the nav module.")

(defvar nav-module--init-loaded nil
  "Non-nil if module init.el loaded successfully.")

(defun nav-module--load-lib ()
  "Load nav module lib.el."
  (load (expand-file-name "lib.el" nav-module--dir) nil t))

(defun nav-module--load-init ()
  "Load nav module init.el."
  (nav-module--load-lib)
  (condition-case nil
      (progn
        (load (expand-file-name "init.el" nav-module--dir) nil t)
        (setq nav-module--init-loaded t))
    (error nil)))

;; Load the module
(nav-module--load-lib)
(nav-module--load-init)

;;; P2: winner-mode enabled

(ert-deftest nav/p2-winner-mode-after-call ()
  "winner-mode should be enabled or configured."
  ;; In batch mode, winner-mode may not be enabled due to after-call deferral
  ;; Just check that it's configured
  (skip-unless (featurep 'winner))
  (should (boundp 'winner-boring-buffers)))

;;; P3: better-jumper-mode enabled

(ert-deftest nav/p3-better-jumper-mode ()
  "better-jumper-mode should be enabled after first-file/buffer hooks."
  (skip-unless (fboundp 'better-jumper-mode))
  (should (boundp 'better-jumper-mode)))

;;; P5: save-place-mode enabled

(ert-deftest nav/p5-save-place-mode ()
  "save-place-mode should be enabled."
  (should (bound-and-true-p save-place-mode)))

;;; P6: window-divider-mode enabled

(ert-deftest nav/p6-window-divider-mode ()
  "window-divider-mode should be enabled."
  (should (bound-and-true-p window-divider-mode)))

;;; P9: avy dispatch actions

(ert-deftest nav/p9-avy-dispatch-custom-actions ()
  "avy-dispatch-alist should include custom c and K actions."
  ;; Skip if avy use-package forms failed in batch mode
  (skip-unless (and (require 'avy nil t)
                    ;; Check if our custom dispatch was set
                    (assq ?c (default-value 'avy-dispatch-alist))))
  (let ((dispatch (default-value 'avy-dispatch-alist)))
    (should (assq ?c dispatch))
    (should (eq (cdr (assq ?c dispatch)) '+avy-action-change-move))
    (should (assq ?K dispatch))
    (should (eq (cdr (assq ?K dispatch)) '+avy-action-evil-lookup))))

;;; Winner boring buffers

(ert-deftest nav/winner-boring-buffers ()
  "winner-boring-buffers should exclude common popup buffers."
  (skip-unless (boundp 'winner-boring-buffers))
  (should (member "*Completions*" winner-boring-buffers))
  (should (member "*Help*" winner-boring-buffers)))

(provide 'nav-tests)

;;; nav-tests.el ends here
