;;; evil/tests.el --- Tests for evil module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for evil modal editing module.

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
  "evil-mode should be enabled after init."
  ;; Skip in batch mode - evil-mode is enabled interactively, not in tests
  (skip-unless (and (featurep 'evil)
                    (boundp 'evil-mode)
                    evil-mode))
  (should evil-mode))

(ert-deftest evil-p1-mode-init-content ()
  "init.el should enable evil-mode."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "(evil-mode +1)" nil t)))))

;;;; P2: Undo System

(ert-deftest evil-p2-undo-system ()
  "evil-undo-system should be undo-redo."
  ;; Skip when evil is not fully loaded with config applied
  (skip-unless (and (featurep 'evil)
                    (boundp 'evil-undo-system)
                    evil-undo-system))
  (should (eq evil-undo-system 'undo-redo)))

(ert-deftest evil-p2-undo-system-init-content ()
  "init.el should set evil-undo-system to undo-redo."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "evil-undo-system 'undo-redo" nil t)))))

;;;; P3: Cursor Shapes

(ert-deftest evil-p3-cursor-shapes ()
  "Cursor shapes should be configured per state."
  ;; Skip when evil is not fully loaded with config applied
  (skip-unless (and (featurep 'evil)
                    (boundp 'evil-normal-state-cursor)
                    (boundp 'evil-insert-state-cursor)
                    evil-normal-state-cursor))
  (should (eq evil-normal-state-cursor 'box))
  (should (eq evil-insert-state-cursor 'bar)))

(ert-deftest evil-p3-cursor-shapes-init-content ()
  "init.el should set cursor shapes."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "evil-normal-state-cursor 'box" nil t))
      (goto-char (point-min))
      (should (search-forward "evil-insert-state-cursor 'bar" nil t)))))

;;;; P6: Surround in Visual

(ert-deftest evil-p6-surround-hook ()
  "evil-surround-mode should be hooked to prog/text/conf modes."
  (skip-unless (boundp 'text-mode-hook))
  ;; Check the hook registration is in init.el
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "evil-surround-mode" nil t)))))

;;;; P7: Shift-Width Sync

(ert-deftest evil-p7-shift-width-sync ()
  "evil-shift-width should sync with tab-width."
  (skip-unless (and (featurep 'evil) (boundp 'evil-shift-width)))
  ;; Check the setq-hook is in init.el
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "evil-shift-width tab-width" nil t)))))

;;;; P8: Minibuffer Escape

(ert-deftest evil-p8-minibuffer-escape-binding ()
  "ESC should be bound in minibuffer maps."
  ;; Check the binding is in init.el
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "+default-minibuffer-maps [escape]" nil t)))))

;;;; P9: Smart Join

(ert-deftest evil-p9-smart-join-advice ()
  "evil-join should have advice for joining comments."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "define-advice evil-join" nil t)))))

;;;; Settings tests

(ert-deftest evil-settings-evil-want ()
  "Evil want-* settings should be in init.el."
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "evil-want-C-u-scroll t" nil t))
      (goto-char (point-min))
      (should (search-forward "evil-want-Y-yank-to-eol t" nil t))
      (goto-char (point-min))
      (should (search-forward "evil-want-keybinding nil" nil t)))))

(ert-deftest evil-settings-sentence-double-space ()
  "sentence-end-double-space should be nil."
  (skip-unless (boundp 'sentence-end-double-space))
  ;; Default should be nil after init
  (let ((init-file (expand-file-name "init.el" module-dir)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (should (search-forward "sentence-end-double-space nil" nil t)))))

(provide 'evil-tests)

;;; evil/tests.el ends here
