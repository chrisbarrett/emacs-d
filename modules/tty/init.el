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

(defun +tty-clear-bg-h (&optional frame)
  "Clear the background on FRAME so the terminal background is used.
When called without FRAME, clear on all TTY frames."
  (if frame
      (set-face-background 'default "unspecified-bg" frame)
    (dolist (f (frame-list))
      (unless (display-graphic-p f)
        (set-face-background 'default "unspecified-bg" f)))))

(add-hook '+after-make-tty-frame-functions #'+tty-clear-bg-h)
(add-hook '+theme-changed-hook #'+tty-clear-bg-h)

(add-transient-hook! '+after-make-tty-frame-functions
  (xterm-mouse-mode +1))

(add-hook '+after-make-tty-frame-functions #'+tty-frame-use-box-characters)

(define-advice tab-bar--update-tab-bar-lines (:after (&rest _) no-tty)
  (dolist (frame (frame-list))
    (unless (display-graphic-p frame)
      (set-frame-parameter frame 'tab-bar-lines 0))))

(add-hook! 'after-init-hook
  (+tty-frame-use-box-characters (selected-frame)))

;;; init.el ends here
