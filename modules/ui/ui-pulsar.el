;;; ui-pulsar.el --- Configuration for pulsar -*- lexical-binding: t; -*-

;;; Commentary:

;; Pulsar provides visual feedback when point moves abruptly to another location.
;; This module configures integrations with various packages.

;;; Code:

(require 'pulsar)

(cl-eval-when (compile)
  (require 'avy)
  (require 'flymake))

;; Add functions that should trigger a pulse
(dolist (fn '(forward-button backward-button isearch-exit))
  (add-to-list 'pulsar-pulse-functions fn))

;; Remove kill-region and delete-region from region pulse functions.
;; These are called internally by puni commands, causing unwanted pulses.
(setq pulsar-pulse-region-functions
      (delq 'kill-region (delq 'delete-region pulsar-pulse-region-functions)))


;;; Jump hooks

(dolist (hook '(consult-after-jump-hook imenu-after-jump-hook))
  (add-hook hook #'pulsar-recenter-top)
  (add-hook hook #'pulsar-reveal-entry))

(dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
  (add-hook hook #'pulsar-recenter-center)
  (add-hook hook #'pulsar-reveal-entry))


;;; Compilation & errors

(setq pulsar-pulse-functions (delq 'next-error pulsar-pulse-functions))
(setq pulsar-pulse-functions (delq 'next-error-recenter pulsar-pulse-functions))
(setq pulsar-pulse-functions (delq 'previous-error pulsar-pulse-functions))

(setq next-error-highlight nil)
(setq next-error-message-highlight t)
(add-hook 'next-error-hook #'pulsar-pulse-line-red)

(with-eval-after-load 'flymake
  (define-advice flymake-goto-next-error (:after (&rest _) pulsar)
    (when pulsar-mode
      (pcase (cl-loop for o in (overlays-at (point))
                      for diag = (overlay-get o 'flymake-diagnostic)
                      when diag
                      return (flymake--severity (flymake-diagnostic-type diag)))
        (3 (pulsar-pulse-line-red))
        (2 (pulsar-pulse-line-yellow))
        (_ (pulsar-pulse-line-cyan))))))


;;; Lisp evaluation

(define-advice eval-region (:around (fn start end &rest args) pulsar)
  "Pulse evaluated regions."
  (+pulsar--with-eval-pulse start end
    (apply fn start end args)))

(define-advice eval-last-sexp (:around (fn &rest args) pulsar)
  "Pulse evaluated expressions."
  (pcase-let ((`(,start . ,end) (or (bounds-of-thing-at-point 'sexp)
                                    (cons (ignore-errors (save-excursion
                                                           (backward-sexp)
                                                           (point)))
                                          (point)))))
    (+pulsar--with-eval-pulse start end
      (apply fn args))))

(define-advice +elisp-eval-buffer (:after (&rest _) pulsar)
  (when pulsar-mode
    (let ((pulse-flag t)
          (pulse-delay pulsar-delay)
          (pulse-iterations pulsar-iterations))
      (pulsar--create-pulse (cons (point-min) (point-max)) 'pulsar-yellow))))


;;; Evil integration

(with-eval-after-load 'evil
  (setq pulsar-pulse-functions (delq 'evil-goto-first-line pulsar-pulse-functions))
  (setq pulsar-pulse-functions (delq 'evil-goto-line pulsar-pulse-functions))
  (dolist (fn '(evil-search-next evil-search-previous))
    (add-to-list 'pulsar-pulse-functions fn))

  (define-advice evil-goto-line (:after (count) pulsar)
    "Don't pulse if moving to the first or last line via gg/G."
    (when (and pulsar-mode
               count
               (< 1 count))
      (pulsar-pulse-line)))

  (define-advice evil-yank (:after (start end &rest _) pulsar)
    "Pulse yanked lines & regions."
    (when pulsar-mode
      (let ((pulse-flag t)
            (pulse-delay pulsar-delay)
            (pulse-iterations pulsar-iterations))
        (pulsar--create-pulse (cons start end) 'pulsar-generic))))

  (define-advice evil-jump-item (:after (&rest _) pulsar)
    "Pulse if jumping to a different line."
    (unless (region-active-p)
      (pulsar-pulse-line))))


;;; Avy integration

(with-eval-after-load 'avy
  (defmacro +pulsar--save-excursion-then-clean-up (form &rest cleanup-forms)
    (declare (indent 1))
    (let ((buf (gensym "buf"))
          (win (gensym "buf")))
      `(let ((,buf (current-buffer))
             (,win (selected-window)))
         ,form
         (with-selected-window ,win
           (with-current-buffer ,buf
             (save-excursion
               ,@cleanup-forms))))))

  (define-advice avy-process (:filter-return (result) pulse-red-on-no-matches)
    (when (eq t result)
      (when pulsar-mode
        (pulsar-pulse-line-red)))
    result)

  (define-advice avy-action-goto (:after (&rest _) pulse)
    (when pulsar-mode
      (pulsar-pulse-line)))

  (defun +avy-pulse-for-change (&rest _)
    (when pulsar-mode
      (pulsar-pulse-line-magenta)))

  (advice-add '+avy-action-change-move :after #'+avy-pulse-for-change)
  (advice-add #'avy-action-kill-move :after #'+avy-pulse-for-change)

  (define-advice avy-action-kill-stay (:around (fn pt) pulse)
    (+pulsar--save-excursion-then-clean-up (funcall fn pt)
      (when pulsar-mode
        (goto-char pt)
        (pulsar-pulse-line-magenta))))

  (defun +avy-pulse-for-action-elsewhere (fn pt)
    (+pulsar--save-excursion-then-clean-up (funcall fn pt)
      (when pulsar-mode
        (goto-char pt)
        (pulsar-pulse-line-green))))

  (advice-add #'avy-action-copy :around #'+avy-pulse-for-action-elsewhere)
  (advice-add '+avy-action-evil-lookup :around #'+avy-pulse-for-action-elsewhere)
  (advice-add #'avy-action-ispell :around #'+avy-pulse-for-action-elsewhere))

(provide 'ui-pulsar)
;;; ui-pulsar.el ends here
