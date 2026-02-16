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
  :commands global-clipetty-mode
  :init
  (add-transient-hook! '+after-make-tty-frame-functions
    (global-clipetty-mode +1)))

;;; TTY frame setup

(add-transient-hook! '+after-make-tty-frame-functions
  (xterm-mouse-mode +1))

(add-hook '+after-make-tty-frame-functions #'+tty-frame-use-box-characters)

(add-hook! 'after-init-hook
  (+tty-frame-use-box-characters (selected-frame)))

;;; init.el ends here
