;;; +pulsar.el --- Configuration for pulsar -*- lexical-binding: t; -*-

;;; Commentary:

;; Pulsar provides visual feedback when point moves abruptly to another location.
;; This module configures integrations with various packages.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'pulsar)

(require '+autoloads)

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


(define-advice flymake-goto-next-error (:after (&rest _) pulsar)
  (when pulsar-mode
    (pcase (cl-loop for o in (overlays-at (point))
                    for diag = (overlay-get o 'flymake-diagnostic)
                    when diag
                    return (flymake--severity (flymake-diagnostic-type diag)))
      (3 (pulsar-pulse-line-red))
      (2 (pulsar-pulse-line-yellow))
      (_ (pulsar-pulse-line-cyan)))))


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
    (pulsar-pulse-line)))


;;; TTY pulse support

;; pulse.el uses (face-background 'default) to determine pulse capability and
;; to compute fade gradients.  On TTY frames where the background is unspecified
;; (for terminal transparency), this disables pulsing entirely.  These advices
;; intercept the background lookup during pulse computation so fades work without
;; changing the actual face attribute.

(defvar +pulse-tty-fallback-bg nil
  "Fallback background color for pulse effects on TTY frames.
When nil, auto-detected from a GUI frame or defaults to black.")

(defun +pulse--tty-fallback-bg ()
  "Return a fallback background color for TTY pulse effects."
  (or +pulse-tty-fallback-bg
      (cl-loop for frame in (frame-list)
               when (display-graphic-p frame)
               return (face-background 'default frame))
      "#000000"))

(defun +pulse--tty-unspecified-bg-p ()
  "Return non-nil if the current frame has an unspecified background."
  (and (not (display-graphic-p))
       (member (face-background 'default) '(nil "unspecified-bg"))))

(define-advice pulse-available-p (:around (fn) tty-24bit)
  "Consider TTY frames with sufficient color support as pulse-capable."
  (or (funcall fn)
      (and (+pulse--tty-unspecified-bg-p)
           (>= (display-color-cells) 256))))

(defvar +pulse--tty-bg-override nil
  "Dynamically bound fallback color during pulse computation.")

(define-advice face-background (:around (fn face &optional frame inherit) pulse-tty-fallback)
  "Return a fallback color for the default face during pulse computation."
  (let ((bg (funcall fn face frame inherit)))
    (if (and +pulse--tty-bg-override
             (eq face 'default)
             (member bg '(nil "unspecified-bg")))
        +pulse--tty-bg-override
      bg)))

(define-advice pulse-momentary-highlight-overlay (:around (fn o &optional face) tty-fallback-bg)
  "Use a fallback background for fade gradient on TTY frames."
  (let ((+pulse--tty-bg-override
         (when (+pulse--tty-unspecified-bg-p)
           (+pulse--tty-fallback-bg))))
    (funcall fn o face)))

;;; +pulsar.el ends here
