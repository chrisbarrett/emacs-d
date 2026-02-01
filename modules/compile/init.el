;;; init.el --- Compilation mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Compilation mode with custom error regex parsers for diverse toolchains.

;;; Code:

(require 'general)
(require '+autoloads)
(require '+corelib)


(use-package compile
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil) ; automatically save before compiling
  (compilation-scroll-output 'first-error)
  (compilation-message-face 'default)
  :general
  (:keymaps '(compilation-mode-map grep-mode-map) :states 'normal
            "{" #'compilation-previous-file
            "}" #'compilation-next-file
            "C-n" #'compilation-next-file
            "C-p" #'compilation-previous-file)
  :config
  (+load "config.el"))


(use-package ansi-color
  :hook (compilation-filter-hook . ansi-color-compilation-filter))


(use-package comint
  :hook
  (compilation-filter-hook comint-truncate-buffer) ; Automatically truncate long compilation buffers.

  :custom
  (comint-prompt-read-only t)
  (comint-buffer-maximum-size 2048)) ; double the default


;; Change to look like a highlighted-line, rather than a visual selection.
(custom-theme-set-faces 'user
                        '(next-error-message ((t (:inherit hl-line)))))


;; Highlight URLs in compilation output & make them navigable.

(use-package compile
  :preface
  (autoload 'goto-address-fontify "goto-addr")
  (defun +compilation-fontify-urls ()
    (goto-address-fontify compilation-filter-start (point)))

  :hook (compilation-filter-hook . +compilation-fontify-urls)

  :general-config
  (:keymaps 'compilation-mode-map
   :states 'normal
   "RET" (general-predicate-dispatch #'compile-goto-error
           (thing-at-point 'url) #'goto-address-at-point)))

(use-package consult-imenu
  :config
  (alist-set! (plist-get (alist-get 'emacs-lisp-mode consult-imenu-config)
                         :types)
              ?P
              '("Parsers" font-lock-variable-name-face)))


;;; init.el ends here
