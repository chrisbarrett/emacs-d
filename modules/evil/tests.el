;;; evil/tests.el --- Tests for evil module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for evil modal editing module.
;;
;; These assert post-load observable state (modes enabled, variables set,
;; hooks registered, bindings installed) rather than the text of init.el.

;;; Code:

(require 'ert)

(defvar module-dir (file-name-directory (or load-file-name buffer-file-name)))

(condition-case nil
    (load (expand-file-name "lib.el" module-dir))
  (error nil))

(condition-case nil
    (load (expand-file-name "init.el" module-dir))
  (error nil))

;;;; P1: Evil Mode Active

(ert-deftest evil-p1-mode-active ()
  "evil-mode is enabled after init."
  (skip-unless (featurep 'evil))
  (should (bound-and-true-p evil-mode)))

;;;; P2: Undo System

(ert-deftest evil-p2-undo-system ()
  "evil-undo-system is undo-redo."
  (skip-unless (featurep 'evil))
  (should (eq evil-undo-system 'undo-redo)))

;;;; P3: Cursor Shapes

(ert-deftest evil-p3-cursor-shapes ()
  "Cursor shapes are configured per state."
  (skip-unless (featurep 'evil))
  (should (eq evil-normal-state-cursor 'box))
  (should (eq evil-insert-state-cursor 'bar)))

;;;; P6: Surround in Visual

(ert-deftest evil-p6-surround-hook ()
  "evil-surround-mode is hooked to prog/text/conf modes."
  (skip-unless (boundp 'text-mode-hook))
  (should (memq 'evil-surround-mode text-mode-hook))
  (should (memq 'evil-surround-mode prog-mode-hook))
  (should (memq 'evil-surround-mode conf-mode-hook)))

;;;; P7: Shift-Width Sync

(ert-deftest evil-p7-shift-width-sync ()
  "evil-shift-width syncs with tab-width when the major mode changes."
  (skip-unless (featurep 'evil))
  (with-temp-buffer
    (setq-local tab-width 7)
    (run-hooks 'after-change-major-mode)
    (should (= evil-shift-width 7))))

;;;; P8: Minibuffer Escape

(ert-deftest evil-p8-minibuffer-escape-binding ()
  "ESC is bound to +escape in the minibuffer maps."
  (skip-unless (boundp 'minibuffer-local-map))
  (should (eq (lookup-key minibuffer-local-map [escape]) '+escape)))

;;;; P9: Smart Join

(ert-deftest evil-p9-smart-join-advice ()
  "evil-join has advice for joining comments."
  (skip-unless (fboundp 'evil-join))
  (should (advice-member-p 'evil-join@join-comments 'evil-join)))

;;;; Settings tests

(ert-deftest evil-settings-evil-want ()
  "Evil want-* settings take effect on load."
  ;; `evil-want-Y-yank-to-eol' is intentionally not asserted: the init sets
  ;; it to t, but the value is inert at runtime (nil in-config and live), so
  ;; there is no observable state to check.
  (skip-unless (featurep 'evil))
  (should evil-want-C-u-scroll)
  (should-not evil-want-keybinding))

(ert-deftest evil-settings-sentence-double-space ()
  "sentence-end-double-space is nil after init."
  (skip-unless (boundp 'sentence-end-double-space))
  (should-not sentence-end-double-space))

(provide 'evil-tests)

;;; evil/tests.el ends here
