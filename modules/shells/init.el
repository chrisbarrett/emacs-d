;;; init.el --- Shells module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Configuration for Eshell and Eat terminal emulator.

;;; Code:

(require '+autoloads)

(require 'cl-lib)
(require 'general)

(require '+corelib)
(load (expand-file-name "../evil/lib" (file-name-directory load-file-name)) nil t)

(cl-eval-when (compile)
  (require 'eat)
  (require 'eshell)
  (require 'evil))

(defvar +eshell-suppress-zoxide-updates-p)
(declare-function +zoxide-add "shells-lib")

;; Eshell configuration - wire zoxide advice when eshell/cd is loaded
(with-eval-after-load 'em-dirs
  (eval-and-compile
    (define-advice eshell/cd (:after (&rest args) update-zoxide)
      "Teach eshell to update Zoxide's index."
      (unless +eshell-suppress-zoxide-updates-p
        (when args
          (+zoxide-add default-directory))))))


;; Eat terminal emulator configuration
(with-eval-after-load 'eat
  ;; Disable evil-mode entirely in eat-mode buffers
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*eat"))))

  ;; Prevent over-scrolling beyond buffer content in eat buffers
  (add-hook 'eat-mode-hook
            (defun +eat-prevent-overscroll-h ()
              "Prevent scrolling beyond buffer content in eat buffers."
              (setq-local scroll-conservatively 101)
              (setq-local maximum-scroll-margin 0.5))))

(with-eval-after-load 'eat
  (general-def
    :keymaps 'eat-semi-char-mode-map
    "s-v" 'eat-yank
    ;; Commands that should be intercepted rather than be passed to eat
    "M-B" nil
    "M-P" nil
    "M-m" nil
    "M-o" nil
    "M-<" nil
    "M->" nil
    "M-_" nil
    ;; Commands that should be passed through
    "C-u" 'eat-self-input
    "C-o" 'eat-self-input
    [escape] 'eat-self-input))

;; Add eat to evil-collection disabled list
(with-eval-after-load 'evil-lib
  (pushnew! +evil-collection-disabled-list 'eat))

;; Eat custom settings
(with-eval-after-load 'eat
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t))



;;; init.el ends here
