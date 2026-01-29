;;; init.el --- Debug module initialization.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Enhancements to the built-in Emacs Lisp debugger for better usability.
;; Uses built-in debug package.

;;; Code:

(require '+autoloads)

(require '+corelib)

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
(use-package debug
  :preface
  (declare-function debugger-frame-clear "debug")
  (declare-function debugger-frame "debug")
  (declare-function debugger-record-buffer "debug")

  ;; Use display-buffer for debugger record buffer.
  (define-advice debugger-record-expression (:after (&rest _) display-buffer)
    (display-buffer debugger-record-buffer))

  :config
  ;; Set mode line in debugger buffers.
  (setq-hook! 'debugger-mode-hook
    mode-line-format +debugger-mode-line-format)

  :general-config
  (:keymaps 'debugger-mode-map
   :states 'normal
   "t" #'+debugger-toggle-on-exit-frame))

