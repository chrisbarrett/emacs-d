;;; init.el --- Terminal compatibility module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Enables GUI-like clipboard and mouse behavior in terminal Emacs.
;; Clipetty provides system clipboard access via OSC 52.
;; Xterm mouse mode enables mouse clicks and scrolling.

;;; Code:

(require '+autoloads)

;; Copy to and paste from system clipboard in terminal via OSC 52.
(with-eval-after-load 'clipetty
  (global-clipetty-mode +1))

(add-hook '+first-input-hook (lambda () (require 'clipetty)))
(add-hook '+first-file-hook (lambda () (require 'clipetty)))

;; Enable mouse support in terminal.
(xterm-mouse-mode +1)



;;; init.el ends here
