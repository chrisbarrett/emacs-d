;;; init.el --- Debug module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhancements to the built-in Emacs Lisp debugger for better usability.
;; Uses built-in debug package.

;;; Code:

(require '+corelib)

;; Toggle debug-on-exit for current frame (combined set/clear).
(defun +debugger-toggle-on-exit-frame ()
  "Toggle whether to reactivate on exit frame."
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

;; Custom mode line showing key reference.
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
     (propertize "  " 'face 'font-lock-builtin-face)
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

;; Debug: Emacs Lisp debugger.
(with-eval-after-load 'debug
  (declare-function debugger-frame-clear "debug")
  (declare-function debugger-frame "debug")
  (declare-function debugger-record-buffer "debug")

  ;; Use display-buffer for debugger record buffer.
  (define-advice debugger-record-expression (:after (&rest _) display-buffer)
    (display-buffer debugger-record-buffer))

  ;; Set mode line in debugger buffers.
  (setq-hook! 'debugger-mode-hook
    mode-line-format +debugger-mode-line-format)

  ;; Keybinding for toggle (requires general and evil).
  (with-eval-after-load 'general
    (with-eval-after-load 'evil
      (general-def :keymaps 'debugger-mode-map
        :states 'normal
        "t" #'+debugger-toggle-on-exit-frame))))

(provide 'debug-init)

;;; init.el ends here
