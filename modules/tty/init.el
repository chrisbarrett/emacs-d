;;; init.el --- Terminal compatibility module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Enables GUI-like clipboard and mouse behavior in terminal Emacs.
;; Clipetty provides system clipboard access via OSC 52.
;; Xterm mouse mode enables mouse clicks and scrolling.

;;; Code:

(require '+autoloads)
(require '+corelib)

;; Copy to and paste from system clipboard in terminal via OSC 52.

(use-package clipetty
  :commands (global-clipetty-mode clipetty-mode)
  :init
  (global-clipetty-mode +1))

;;; TTY frame setup

(add-transient-hook! '+after-make-tty-frame-functions
  (xterm-mouse-mode +1))

(add-hook '+after-make-tty-frame-functions #'+tty-frame-use-box-characters)

(add-hook! 'after-init-hook
  (+tty-frame-use-box-characters (selected-frame)))

;; The special value "unspecified-bg" is used by the Emacs rendering system to
;; avoid painting over the terminal's default background. We need to use this in
;; TTY contexts so active vs inactive background colours are applied in Tmux.

;; Start by caching the theme's original background color setting--we need this

(defvar +theme-default-background (face-background 'default)
  "Cached default background colour before it is cleared for TTY setup.")

(defvar +tty-clear-background "unspecified-bg"
  "Special background colour for terminal background pass-through.")

(defun +tty-save-default-bg-h ()
  "Save the background colour."
  (when-let* ((color (face-background 'default)))
    (unless (equal +tty-clear-background color)
      (setq +theme-default-background color))))

(add-hook '+theme-changed-hook #'+tty-save-default-bg-h -95)

;; Now that it's cached and set to be updated, we ensure it's cleared in TTY
;; frames on creation, and updated every time the theme changes.

(defun +tty-clear-bg-h (&optional frame)
  "Clear the background on FRAME so the terminal background is used.
When called without FRAME, clear on all TTY frames."
  (if frame
      (unless (display-graphic-p frame)
        (set-face-background 'default +tty-clear-background frame))
    (dolist (f (frame-list))
      (unless (display-graphic-p f)
        (set-face-background 'default +tty-clear-background f)))))

(add-hook '+after-make-tty-frame-functions #'+tty-clear-bg-h)
(add-hook '+theme-changed-hook #'+tty-clear-bg-h)

;; pulse.el needs to know the original background colour in TTYs would be so it
;; can interpolate.

(defvar +has-true-color (equal "truecolor" (getenv "COLORTERM")))

(define-advice pulse-available-p (:filter-return (res) truecolor-tty)
  (or res +has-true-color))

(defun +face-background-resolve-original-value-a (fn &rest args)
  (cl-letf* ((orig (symbol-function 'face-background))
             ((symbol-function 'face-background)
              (lambda (face &optional frame inherit)
                (if (equal face 'default)
                    +theme-default-background
                  (funcall orig face frame inherit)))))
    (apply fn args)))

(advice-add #'pulse-momentary-highlight-overlay :around #'+face-background-resolve-original-value-a)
(advice-add #'winpulse-window :around #'+face-background-resolve-original-value-a)


;;; init.el ends here
