;;; mod-pulsar.el --- Configuration for pulsar -*- lexical-binding: t; -*-

;;; Commentary:

;; I use pulsar to provide a visual indicator when point moves abruptly to
;; another location--this is especially helpful when there's a significant
;; change in context, such as jumping to a different buffer or window.
;;
;; Pulsar requires a bit of elbow-grease to work well with some packages.

;;; Code:

(require 'pulsar)
(require '+corelib)

(cl-eval-when (compile)
  (require 'avy)
  (require 'flymake))

(pushnew! pulsar-pulse-functions 'forward-button 'backward-button 'isearch-exit)

;; Remove kill-region and delete-region from region pulse functions.
;; These are called internally by puni commands (e.g., puni-backward-kill-word),
;; which causes unwanted pulses during routine editing.
(delq! 'kill-region pulsar-pulse-region-functions)
(delq! 'delete-region pulsar-pulse-region-functions)

(dolist (hook '(consult-after-jump-hook imenu-after-jump-hook))
  (add-hook hook #'pulsar-recenter-top)
  (add-hook hook #'pulsar-reveal-entry))

(dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
  (add-hook hook #'pulsar-recenter-center)
  (add-hook hook #'pulsar-reveal-entry))


;;; compilation & errors

(delq! 'next-error pulsar-pulse-functions)
(delq! 'next-error-recenter pulsar-pulse-functions)
(delq! 'previous-error pulsar-pulse-functions)

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

(defmacro +pulsar--with-eval-pulse (start end &rest body)
  "Pulse a region to indicate whether a command ran or signalled an error.

START and END are the buffer locations to pulse after evaluating BODY.

START & END are evaluated after BODY has completed, and thus after any
buffer modifications have happened."
  (declare (indent 2))
  (let ((gfailed (gensym "failed-"))
        (gerr (gensym "err-"))
        (gstart (gensym "start-"))
        (gend (gensym "end-")))
    `(let (,gfailed)
       (unwind-protect
           (condition-case ,gerr
               (progn ,@body)
             (t
              (setq ,gfailed t)
              (signal (car ,gerr) (cdr ,gerr))))
         (deactivate-mark)
         (when pulsar-mode
           (let ((,gstart ,start)
                 (,gend ,end))
             (when (and ,gstart ,gend)
               (let ((pulse-flag t)
                     (pulse-delay pulsar-delay)
                     (pulse-iterations pulsar-iterations))
                 (pulsar--create-pulse (cons ,gstart ,gend)
                                       (if ,gfailed 'pulsar-red 'pulsar-green))))))))))


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


;;; evil

(with-eval-after-load 'evil

  (delq! 'evil-goto-first-line pulsar-pulse-functions)
  (delq! 'evil-goto-line pulsar-pulse-functions)
  (pushnew! pulsar-pulse-functions 'evil-search-next 'evil-search-previous )

  (define-advice evil-goto-line (:after (count) pulsar)
    "Don't pulse if moving to the first or last line via gg/G."
    (when (and pulsar-mode
               count ; nil if going to end of buffer
               (< 1 count ))
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
      (pulsar-pulse-line)))
  )


;;; avy

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
  (advice-add #'avy-action-ispell :around #'+avy-pulse-for-action-elsewhere)
  )



(provide 'mod-pulsar)

;;; mod-pulsar.el ends here
