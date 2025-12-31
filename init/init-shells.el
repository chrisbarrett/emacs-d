;;; init-shells.el --- Shell hosted inside Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


;; Emacs' built-in shell combining Emacs Lisp evaluation with Unix shell
;; features.
(use-package eshell
  :config
  (use-package mod-eshell :demand t))

;; A reasonably performant terminal emulator in Emacs
(use-package eat :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  :general
  (:keymaps 'eat-semi-char-mode-map
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
            [escape] 'eat-self-input)
  :init
  (with-eval-after-load '+evil-collection
    (pushnew! +evil-collection-disabled-list 'eat))

  :config
  ;; Disable evil-mode entirely in eat-mode buffers.
  (with-eval-after-load 'evil
    (pushnew! evil-buffer-regexps `(,(rx bol "*eat"))))

  ;; Prevent over-scrolling beyond buffer content in eat buffers
  (add-hook 'eat-mode-hook
            (defun +eat-prevent-overscroll-h ()
              "Prevent scrolling beyond buffer content in eat buffers."
              (setq-local scroll-conservatively 101)
              (setq-local maximum-scroll-margin 0.5))))


(provide 'init-shells)

;;; init-shells.el ends here
