;;; hex/tests.el --- Tests for hex module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for vim-style hexl mode keybindings.

;;; Code:

(require 'ert)

(defvar hex-tests--init-loaded nil
  "Whether init.el has been loaded.")

(condition-case nil
    (let ((init-file (expand-file-name
                      "init.el"
                      (file-name-directory (or load-file-name buffer-file-name)))))
      (load init-file nil t)
      (setq hex-tests--init-loaded t))
  (error nil))

;; P1: h bound to hexl-backward-char in hexl-mode normal state
(ert-deftest hex-test-h-keybinding ()
  "Test h keybinding for hexl-backward-char."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "h") #'hexl-backward-char)))))

;; P2: l bound to hexl-forward-char in hexl-mode normal state
(ert-deftest hex-test-l-keybinding ()
  "Test l keybinding for hexl-forward-char."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "l") #'hexl-forward-char)))))

;; P3: ]] bound to hexl-end-of-1k-page in hexl-mode normal state
(ert-deftest hex-test-bracket-bracket-keybinding ()
  "Test ]] keybinding for hexl-end-of-1k-page."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "]]") #'hexl-end-of-1k-page)))))

;; P4: [[ bound to hexl-beginning-of-1k-page in hexl-mode normal state
(ert-deftest hex-test-bracket-open-keybinding ()
  "Test [[ keybinding for hexl-beginning-of-1k-page."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "[[") #'hexl-beginning-of-1k-page)))))

;; P5: $ bound to hexl-end-of-line in hexl-mode normal state
(ert-deftest hex-test-dollar-keybinding ()
  "Test $ keybinding for hexl-end-of-line."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "$") #'hexl-end-of-line)))))

;; P6: 0 and ^ both bound to hexl-beginning-of-line in hexl-mode normal state
(ert-deftest hex-test-zero-caret-keybindings ()
  "Test 0 and ^ keybindings for hexl-beginning-of-line."
  (skip-unless hex-tests--init-loaded)
  (skip-unless (and (boundp 'general-define-key) (fboundp 'general-define-key)))
  (require 'hexl)
  (with-temp-buffer
    (hexl-mode)
    (let ((binding (evil-get-minor-mode-keymap 'normal 'hexl-mode)))
      (skip-unless binding)
      (should (eq (lookup-key binding "0") #'hexl-beginning-of-line))
      (should (eq (lookup-key binding "^") #'hexl-beginning-of-line)))))

;; Additional test: hexl commands are defined
(ert-deftest hex-test-commands-defined ()
  "Test that hexl commands referenced in keybindings exist."
  (require 'hexl)
  (should (fboundp 'hexl-backward-char))
  (should (fboundp 'hexl-forward-char))
  (should (fboundp 'hexl-end-of-1k-page))
  (should (fboundp 'hexl-beginning-of-1k-page))
  (should (fboundp 'hexl-end-of-line))
  (should (fboundp 'hexl-beginning-of-line)))

(provide 'hex-tests)

;;; hex/tests.el ends here
