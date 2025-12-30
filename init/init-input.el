;;; init-input.el --- User input configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

(use-package general
  :general-config (:keymaps +default-minibuffer-maps "s-v" #'yank))

;; Insert non-breaking space on C-c SPC

(keymap-global-set "C-c SPC"
                   (defun +insert-nbsp ()
                     (interactive)
                     (insert-char #x00A0)))

;; Teach Emacs that C-i and C-m do in fact exist.

(pcase-dolist (`(,key ,fallback . ,events)
               '(([C-i] [?\C-i] tab kp-tab)
                 ([C-m] [?\C-m] return kp-return)))
  (define-key
   input-decode-map fallback
   (lambda (&rest _args)
     (interactive)
     (if (when-let* ((keys (this-single-command-raw-keys)))
           (and (display-graphic-p)
                (not (cl-loop for event in events
                              if (cl-position event keys)
                              return t))
                ;; Use FALLBACK if nothing is bound to KEY, otherwise we've
                ;; broken all pre-existing FALLBACK keybinds.
                (key-binding (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                      key)
                             nil t)))
         key fallback))))


;; Emacs' core paragraph parser.
(use-package paragraphs
  :custom
  (sentence-end-double-space nil))


;; Provides commands for cycling different string casing styles for the ident
;; at point, e.g. UpperCamelCase, lowerCamelCase, etc.
(use-package string-inflection :ensure t
  :general (:states '(normal insert) "M--" #'string-inflection-all-cycle))


;; Provides comment-related commands and variables to customise their
;; behaviour.
(use-package newcomment
  :custom
  (comment-empty-lines t)
  (comment-multi-line t)
  (comment-style 'extra-line)
  :config
  (setq-default comment-column 0))


(use-package mod-input-methods
  :init
  (keymap-global-set "M-i" 'activate-transient-input-method)
  :after-call toggle-input-method activate-transient-input-method)


;; Automatically insert matching pairs.
(use-package elec-pair
  :after-call +first-file-hook +first-buffer-hook
  :init
  (electric-pair-mode +1))


;; Provides structured editing commands.
(use-package puni :ensure t
  :after-call +first-file-hook +first-buffer-hook
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general-config
  (:keymaps 'puni-mode-map :states 'insert "C-w" #'puni-backward-kill-word)
  (:keymaps 'puni-mode-map :states '(visual) "C-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "C-k" #'+kill-line
            "M-(" #'puni-wrap-round "M-)" #'puni-wrap-round
            "M-]" #'puni-wrap-square ;; NB. M-[ translates to the ESC-[ control sequence in terminals.
            "M-{" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-previous))
            "M-}" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-next))))


;; Despite its name, provides many programming-language generic features.
(use-package lisp
  :general
  (:keymaps 'prog-mode-map :states 'normal "(" 'backward-sexp ")" 'forward-sexp))


(provide 'init-input)

;;; init-input.el ends here
