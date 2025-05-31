;;; mod-debug.el --- Configuration for the built-in Lisp debugger. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'debug)
(require 'general)

;; Make 't' toggle whether to reactivate on frame exit; having separate
;; keybindings for setting and clearing this state isn't what I want.

(defun +debugger-toggle-on-exit-frame ()
  (interactive)
  (let ((enabled-for-line (save-excursion
                            (goto-char (line-beginning-position))
                            (looking-at (rx (* space) "*" (+ space))))))
    (cond
     (enabled-for-line
      (debugger-frame-clear)
      (message "debug on exit for frame disabled"))
     (t
      (debugger-frame)
      (message "debug on exit for frame enabled")))))

(general-def :keymaps 'debugger-mode-map :states 'normal
  "t" #'+debugger-toggle-on-exit-frame)

;; Use display-buffer to ensure the debugger record buffer ends up in an
;; appropriate window.

(define-advice debugger-record-expression (:after (&rest _) display-buffer)
  (display-buffer debugger-record-buffer))



;; Show keybindings in the header line for Backtrace buffers.

(defconst +debugger-mode-line-format
  (cl-labels ((low-emphasis (str)
                (propertize str 'face 'parenthesis))
              (key (key desc)
                (concat (propertize key 'face 'which-key-key-face) (low-emphasis ":") " " desc))
              (group (&rest children)
                (concat (low-emphasis "|") "  " (apply #'distribute children)))
              (distribute (&rest strs)
                (string-join strs "  ")))
    (distribute
     (propertize "  " 'face 'font-lock-builtin-face)
     (group
      (key "d" "step")
      (key "c" "continue")
      (key "r" "return"))
     (group
      (key "t" "toggle debug on exit frame")
      (key "J" "jump")
      (key "L" "locals"))
     (group
      (key "E" "eval")
      (key "R" "eval & record")))))

(setq-hook! 'debugger-mode-hook
  mode-line-format +debugger-mode-line-format)

(provide 'mod-debug)

;;; mod-debug.el ends here
