;;; hex/tests.el --- Tests for hex module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vim-style hexl mode keybindings.

;;; Code:

(require 'ert)
(require 'evil)
(require 'hexl)
(require 'general)

(let ((init-file (expand-file-name
                  "init.el"
                  (file-name-directory (or load-file-name buffer-file-name)))))
  (load init-file nil t))

(defun hex-tests--lookup (key)
  "Return the normal-state binding for KEY in `hexl-mode-map'."
  (lookup-key (evil-get-auxiliary-keymap hexl-mode-map 'normal t t) key))

(ert-deftest hex-test-keybindings ()
  "Vim-style movement keys are bound to hexl commands in normal state."
  (should (eq (hex-tests--lookup "h")  #'hexl-backward-char))
  (should (eq (hex-tests--lookup "l")  #'hexl-forward-char))
  (should (eq (hex-tests--lookup "]]") #'hexl-end-of-1k-page))
  (should (eq (hex-tests--lookup "[[") #'hexl-beginning-of-1k-page))
  (should (eq (hex-tests--lookup "$")  #'hexl-end-of-line))
  (should (eq (hex-tests--lookup "0")  #'hexl-beginning-of-line))
  (should (eq (hex-tests--lookup "^")  #'hexl-beginning-of-line)))

(provide 'hex-tests)

;;; hex/tests.el ends here
