;;; evil-tty-cursor.el --- Change cursor shape based on evil state in TTY -*- lexical-binding: t; -*-

;;; Commentary:

;; This package provides cursor shape changes for evil states in TTY terminals.
;; It uses modern DECSCUSR sequences that work with Alacritty, kitty, ghostty,
;; and other modern terminal emulators.

;;; Code:

(require 'evil)

(defgroup evil-tty-cursor nil
  "Change cursor shape based on evil state in TTY."
  :group 'evil
  :prefix "evil-tty-cursor-")

(defcustom evil-tty-cursor-states
  '((normal . box)
    (insert . bar)
    (visual . hollow)
    (replace . hbar)
    (operator . hbar)
    (emacs . hollow))
  "Mapping of evil states to cursor shapes.
Cursor shapes can be: box, bar, hbar, hollow."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil-tty-cursor)

;;; Internal functions

(defun evil-tty-cursor--make-sequence (shape)
  "Create DECSCUSR sequence for cursor SHAPE.
Uses modern terminal standard escape sequences."
  (pcase shape
    ('box "\e[2 q")      ; Steady block
    ('bar "\e[6 q")      ; Steady bar
    ('hbar "\e[4 q")     ; Steady underline
    ('hollow "\e[2 q")   ; Steady block (fallback for hollow)
    (_ "\e[2 q")))       ; Default to block

(defun evil-tty-cursor--update-cursor ()
  "Update cursor shape based on current evil state."
  (when (and (bound-and-true-p evil-tty-cursor-mode)
             (not (display-graphic-p)))
    (when-let* ((shape (alist-get evil-state evil-tty-cursor-states))
                (sequence (evil-tty-cursor--make-sequence shape)))
      (send-string-to-terminal sequence))))

(defun evil-tty-cursor--enable-hooks ()
  "Enable evil state change hooks."
  (add-hook 'evil-normal-state-entry-hook #'evil-tty-cursor--update-cursor)
  (add-hook 'evil-insert-state-entry-hook #'evil-tty-cursor--update-cursor)
  (add-hook 'evil-visual-state-entry-hook #'evil-tty-cursor--update-cursor)
  (add-hook 'evil-replace-state-entry-hook #'evil-tty-cursor--update-cursor)
  (add-hook 'evil-operator-state-entry-hook #'evil-tty-cursor--update-cursor)
  (add-hook 'evil-emacs-state-entry-hook #'evil-tty-cursor--update-cursor))

(defun evil-tty-cursor--disable-hooks ()
  "Disable evil state change hooks."
  (remove-hook 'evil-normal-state-entry-hook #'evil-tty-cursor--update-cursor)
  (remove-hook 'evil-insert-state-entry-hook #'evil-tty-cursor--update-cursor)
  (remove-hook 'evil-visual-state-entry-hook #'evil-tty-cursor--update-cursor)
  (remove-hook 'evil-replace-state-entry-hook #'evil-tty-cursor--update-cursor)
  (remove-hook 'evil-operator-state-entry-hook #'evil-tty-cursor--update-cursor)
  (remove-hook 'evil-emacs-state-entry-hook #'evil-tty-cursor--update-cursor))

;;;###autoload
(define-minor-mode evil-tty-cursor-mode
  "Change cursor shape based on evil state in TTY terminals."
  :init-value nil
  :lighter nil
  :group 'evil-tty-cursor
  (cond (evil-tty-cursor-mode
         (evil-tty-cursor--enable-hooks)
         (evil-tty-cursor--update-cursor))
        (t
         (evil-tty-cursor--disable-hooks))))

(defun evil-tty-cursor--maybe-enable ()
  "Enable `evil-tty-cursor-mode' if appropriate."
  (when (and (bound-and-true-p evil-mode)
             (not (display-graphic-p)))
    (evil-tty-cursor-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-evil-tty-cursor-mode
  evil-tty-cursor-mode evil-tty-cursor--maybe-enable
  :group 'evil-tty-cursor)

(provide 'evil-tty-cursor)

;;; evil-tty-cursor.el ends here
