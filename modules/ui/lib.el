;;; lib.el --- UI module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for the ui module.

;;; Code:

(require 'cl-lib)


;;; Tab bar alert functions

(defvar +tab-bar-alert-clear-delay 1.0
  "Seconds to wait before clearing alert state on a tab.")

(defvar +tab-bar-alert-pulse-iterations 3
  "Number of times to pulse the alert.")

(defvar +tab-bar-alert-pulse-delay 0.05
  "Delay in seconds between pulse steps.")

;;;###autoload
(defun +tab-bar-set-alert (&optional tab-name)
  "Set alert state on TAB-NAME (or current tab if nil).
This will cause the tab to display with a visually distinct background
until the user dwells on it for `+tab-bar-alert-clear-delay' seconds."
  (interactive)
  (require 'ui-tabs)
  (+tab-bar-set-alert--impl tab-name))

;;;###autoload
(defun +tab-bar-clear-alert (&optional tab-name)
  "Clear alert state from TAB-NAME (or current tab if nil).
Fades out the alert before removing it."
  (interactive)
  (require 'ui-tabs)
  (+tab-bar-clear-alert--impl tab-name))

;;;###autoload
(defun +tab-bar-set-transient-alert (&optional tab-name color cycles)
  "Set a transient alert on TAB-NAME (or current tab if nil).
COLOR is the pulse color (default pulsar-magenta).
CYCLES is the number of pulses (default 3)."
  (interactive)
  (require 'ui-tabs)
  (+tab-bar-set-transient-alert--impl tab-name color cycles))

;;;###autoload
(defun +tabs-menu ()
  "Transient menu for tab operations."
  (interactive)
  (require 'ui-tabs)
  (call-interactively #'+tabs-menu--impl))

;;;###autoload
(defun +update-tab-bar-themes (&rest _)
  "Update tab-bar colors to be distinct and theme-aware."
  (require 'ui-tabs)
  (+update-tab-bar-themes--impl))


;;; Pulsar macro

;;;###autoload
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
         (when (and (bound-and-true-p pulsar-mode)
                    (require 'pulsar nil t))
           (let ((,gstart ,start)
                 (,gend ,end))
             (when (and ,gstart ,gend)
               (let ((pulse-flag t)
                     (pulse-delay pulsar-delay)
                     (pulse-iterations pulsar-iterations))
                 (pulsar--create-pulse (cons ,gstart ,gend)
                                       (if ,gfailed 'pulsar-red 'pulsar-green))))))))))


;;; Display buffer functions

;;;###autoload
(defun +display-buffer-reuse-non-dedicated-window (buffer alist)
  "Reuse a non-dedicated window for BUFFER when navigating.
Only works for specific navigation commands like dired-find-file,
next-error, etc.  ALIST is display-buffer action alist."
  (when (member this-command '(dired-find-file
                               next-error
                               previous-error
                               compile-goto-error
                               push-button
                               magit-diff-visit-file
                               +magit-diff-visit-file-unselected))
    (let ((candidates (seq-remove (lambda (it)
                                    (or (+side-window-p it)
                                        (window-dedicated-p it)))
                                  (window-list))))
      (pcase candidates
        (`(,sole-window)
         (window--display-buffer buffer sole-window 'reuse alist))))))

;;;###autoload
(defun +display-buffer-fallback (buffer &rest _)
  "Sensible split fallback for displaying BUFFER."
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window)))
    t))


;;; TTY frame setup

;;;###autoload
(defun +tty-frame-setup (frame)
  "On TTY FRAME, use Unicode box-drawing for window separators."
  (unless (display-graphic-p frame)
    (with-selected-frame frame
      (let ((dt (or (frame-parameter frame '+vborder-dtable)
                    (let ((dt (make-display-table)))
                      (set-display-table-slot dt 'vertical-border (make-glyph-code ?│))
                      (set-frame-parameter frame '+vborder-dtable dt)
                      dt))))
        (set-display-table-slot dt 'truncation (make-glyph-code ?… 'warning))

        (dolist (window (window-list frame 'no-minibuf))
          (set-window-display-table window dt))

        (let ((update-display-table (lambda ()
                                      (when (eq (selected-frame) frame)
                                        (dolist (window (window-list frame 'no-minibuf))
                                          (unless (eq (window-display-table window) dt)
                                            (set-window-display-table window dt)))))))
          (with-current-buffer (window-buffer (frame-selected-window frame))
            (add-hook 'window-configuration-change-hook update-display-table nil t)))))))


;;; Goto address helper

;;;###autoload
(defun +goto-address-maybe-h ()
  "Enable goto-address-mode unless in org-mode (which handles URLs natively)."
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (goto-address)
    (goto-address-mode +1)))

(provide 'lib)
;;; lib.el ends here
