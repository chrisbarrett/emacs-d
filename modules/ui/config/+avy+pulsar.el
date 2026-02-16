;;; +avy+pulsar.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'avy)
(require 'pulsar)

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


(defun +avy+pulsar-pulse-for-change (&rest _)
  (when pulsar-mode
    (pulsar-pulse-line-magenta)))

(defun +avy+pulsar-pulse-for-action-elsewhere (fn pt)
  (+pulsar--save-excursion-then-clean-up (funcall fn pt)
                                         (when pulsar-mode
                                           (goto-char pt)
                                           (pulsar-pulse-line-green))))

(define-advice avy-process (:filter-return (result) pulse-red-on-no-matches)
  (when (eq t result)
    (when pulsar-mode
      (pulsar-pulse-line-red)))
  result)

(define-advice avy-action-goto (:after (&rest _) pulse)
  (when pulsar-mode
    (pulsar-pulse-line)))

(advice-add '+avy+pulsar-action-change-move :after #'+avy+pulsar-pulse-for-change)
(advice-add #'avy-action-kill-move :after #'+avy+pulsar-pulse-for-change)

(define-advice avy-action-kill-stay (:around (fn pt) pulse)
  (+pulsar--save-excursion-then-clean-up (funcall fn pt)
                                         (when pulsar-mode
                                           (goto-char pt)
                                           (pulsar-pulse-line-magenta))))

(advice-add #'avy-action-copy :around #'+avy+pulsar-pulse-for-action-elsewhere)
(advice-add '+avy+pulsar-action-evil-lookup :around #'+avy+pulsar-pulse-for-action-elsewhere)
(advice-add #'avy-action-ispell :around #'+avy+pulsar-pulse-for-action-elsewhere)

;;; +avy+pulsar.el ends here
