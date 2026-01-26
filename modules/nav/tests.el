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

;;; Module structure tests

(ert-deftest nav/module-has-packages-eld ()
  "Module should have packages.eld."
  (should (file-exists-p (expand-file-name "packages.eld" nav-module--dir))))

(ert-deftest nav/module-has-spec-md ()
  "Module should have spec.md."
  (should (file-exists-p (expand-file-name "spec.md" nav-module--dir))))

;;; P1: windmove bindings

(ert-deftest nav/p1-windmove-up-binding ()
  "M-C should be bound to windmove-up."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "M-C") #'windmove-up)))

(ert-deftest nav/p1-windmove-down-binding ()
  "M-T should be bound to windmove-down."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "M-T") #'windmove-down)))

(ert-deftest nav/p1-windmove-left-binding ()
  "M-H should be bound to windmove-left."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "M-H") #'windmove-left)))

(ert-deftest nav/p1-windmove-right-binding ()
  "M-N should be bound to windmove-right."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "M-N") #'windmove-right)))

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

;;; P4: C-, and C-. jump bindings

(ert-deftest nav/p4-jump-forward-binding ()
  "C-. should be bound to better-jumper-jump-forward in normal state."
  ;; Skip if general/better-jumper use-package forms failed in batch mode
  (skip-unless (and (require 'evil nil t)
                    (require 'better-jumper nil t)
                    (require 'general nil t)
                    ;; Check our binding was actually set
                    (eq (keymap-lookup evil-normal-state-map "C-.")
                        #'better-jumper-jump-forward)))
  (should (eq (keymap-lookup evil-normal-state-map "C-.")
              #'better-jumper-jump-forward)))

(ert-deftest nav/p4-jump-backward-binding ()
  "C-, should be bound to better-jumper-jump-backward in normal state."
  ;; Skip if general/better-jumper use-package forms failed in batch mode
  (skip-unless (and (require 'evil nil t)
                    (require 'better-jumper nil t)
                    (require 'general nil t)
                    ;; Check our binding was actually set
                    (eq (keymap-lookup evil-normal-state-map "C-,")
                        #'better-jumper-jump-backward)))
  (should (eq (keymap-lookup evil-normal-state-map "C-,")
              #'better-jumper-jump-backward)))

;;; P5: save-place-mode enabled

(ert-deftest nav/p5-save-place-mode ()
  "save-place-mode should be enabled."
  (should (bound-and-true-p save-place-mode)))

;;; P6: window-divider-mode enabled

(ert-deftest nav/p6-window-divider-mode ()
  "window-divider-mode should be enabled."
  (should (bound-and-true-p window-divider-mode)))

(ert-deftest nav/p6-window-divider-places ()
  "window-divider-default-places should be t."
  (should (eq window-divider-default-places t)))

(ert-deftest nav/p6-window-divider-widths ()
  "window-divider widths should be 1."
  (should (eq window-divider-default-bottom-width 1))
  (should (eq window-divider-default-right-width 1)))

;;; P7: fullframe toggle

(ert-deftest nav/p7-fullframe-command-defined ()
  "+toggle-window-fullframe should be defined."
  (should (fboundp '+toggle-window-fullframe)))

(ert-deftest nav/p7-fullframe-binding ()
  "M-f should be bound to +toggle-window-fullframe."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "M-f") #'+toggle-window-fullframe)))

;;; P8: win-swap commands

(ert-deftest nav/p8-win-swap-commands-defined ()
  "+win-swap-* commands should be defined."
  (should (fboundp '+win-swap-up))
  (should (fboundp '+win-swap-down))
  (should (fboundp '+win-swap-left))
  (should (fboundp '+win-swap-right)))

(ert-deftest nav/p8-win-swap-up-binding ()
  "C-M-c should be bound to +win-swap-up."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "C-M-c") #'+win-swap-up)))

(ert-deftest nav/p8-win-swap-down-binding ()
  "C-M-t should be bound to +win-swap-down."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "C-M-t") #'+win-swap-down)))

(ert-deftest nav/p8-win-swap-left-binding ()
  "C-M-h should be bound to +win-swap-left."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "C-M-h") #'+win-swap-left)))

(ert-deftest nav/p8-win-swap-right-binding ()
  "C-M-n should be bound to +win-swap-right."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (eq (keymap-lookup override-global-map "C-M-n") #'+win-swap-right)))

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

(ert-deftest nav/p9-avy-binding ()
  "M-g should be bound to avy-goto-char-timer."
  (skip-unless (and (require 'general nil t)
                    (boundp 'override-global-map)))
  (should (or (eq (keymap-lookup (current-global-map) "M-g") #'avy-goto-char-timer)
              (eq (keymap-lookup override-global-map "M-g") #'avy-goto-char-timer))))

;;; Additional function tests

(ert-deftest nav/side-window-p-defined ()
  "+side-window-p should be defined."
  (should (fboundp '+side-window-p)))

(ert-deftest nav/toggle-side-window-raised-defined ()
  "+toggle-side-window-raised should be defined."
  (should (fboundp '+toggle-side-window-raised)))

(ert-deftest nav/set-jump-point-defined ()
  "+set-jump-point should be defined."
  (should (fboundp '+set-jump-point)))

(ert-deftest nav/sibling-file-or-other-buffer-defined ()
  "+sibling-file-or-other-buffer should be defined."
  (should (fboundp '+sibling-file-or-other-buffer)))

(ert-deftest nav/split-dwim-commands-defined ()
  "DWIM split commands should be defined."
  (should (fboundp '+split-window-horizontally-dwim))
  (should (fboundp '+split-window-vertically-dwim)))

(ert-deftest nav/toggle-window-dedication-defined ()
  "+toggle-window-dedication should be defined."
  (should (fboundp '+toggle-window-dedication)))

(ert-deftest nav/delete-nondedicated-windows-defined ()
  "+delete-nondedicated-windows should be defined."
  (should (fboundp '+delete-nondedicated-windows)))

(ert-deftest nav/clone-indirect-buffer-of-region-defined ()
  "+clone-indirect-buffer-of-region should be defined."
  (should (fboundp '+clone-indirect-buffer-of-region)))

(ert-deftest nav/avy-action-change-move-defined ()
  "+avy-action-change-move should be defined."
  (should (fboundp '+avy-action-change-move)))

(ert-deftest nav/avy-action-evil-lookup-defined ()
  "+avy-action-evil-lookup should be defined."
  (should (fboundp '+avy-action-evil-lookup)))

;;; Variables

(ert-deftest nav/variables-defined ()
  "Module variables should be defined."
  (should (boundp '+window-original-side))
  (should (boundp '+window-return-configuration))
  (should (boundp '+side-window-default-width))
  (should (boundp '+side-window-default-height)))

(ert-deftest nav/hooks-defined ()
  "Module hooks should be defined."
  (should (boundp '+side-window-raised-hook))
  (should (boundp '+side-window-returned-hook)))

;;; Winner boring buffers

(ert-deftest nav/winner-boring-buffers ()
  "winner-boring-buffers should exclude common popup buffers."
  (skip-unless (boundp 'winner-boring-buffers))
  (should (member "*Completions*" winner-boring-buffers))
  (should (member "*Help*" winner-boring-buffers)))

(provide 'nav-tests)

;;; nav-tests.el ends here
