;;; init-evil.el --- Configuration for evil-mode (Vim-style modal editing) -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Since I use evil, I have no need for the usual rectangular selection
;; keybinding.
(keymap-global-set "C-x SPC"
                   (defun +insert-char ()
                     "Insert a character at point."
                     (interactive)
                     (evil-insert-state)
                     (call-interactively
                      (if (equal system-type 'darwin)
                          #'ns-do-show-character-palette
                        #'insert-char))))


;; Evil is a better vim emulation implementation than the one that
;; ships with Emacs.
(use-package evil :ensure t
  :demand t
  :general-config
  (:states 'emacs
           "ESC ESC" #'evil-normal-state)
  (:states '(insert normal emacs)
           "M-." #'xref-find-definitions
           "C-x RET" #'insert-char)
  (:states 'visual
           "<tab>" 'indent-region
           "TAB" 'indent-region)
  :custom
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-v$-excludes-newline t)
  (evil-want-C-g-bindings)
  (evil-want-C-u-delete nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-integration t)
  (evil-want-keybinding nil)

  :config
  (use-package mod-evil :demand t)
  (evil-mode +1)

  (add-hook '+escape-hook
            (defun +evil-disable-ex-highlights-h ()
              (when (evil-ex-hl-active-p 'evil-ex-search)
                (evil-ex-nohighlight)
                t)))

  :general-config
  (:keymaps +default-minibuffer-maps
            "C-a"    #'move-beginning-of-line
            "C-r"    #'evil-paste-from-register
            "C-u"    #'evil-delete-back-to-indentation
            "C-v"    #'yank
            "C-w"    (defun +delete-backward-word-no-kill (arg)
                       "Like `backward-kill-word', but doesn't affect the kill-ring."
                       (interactive "p")
                       (let ((kill-ring nil) (kill-ring-yank-pointer nil))
                         (ignore-errors (backward-kill-word arg))))))


;; Visualise the Emacs undo history.
(use-package vundo :ensure (vundo :host github :repo "casouri/vundo")
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


;; Community-managed collection of evil keybindings; makes evil behave more
;; consistently across many modes.
(use-package evil-collection :ensure t
  :custom
  ;; Ensure we do not overwrite the global leader key binding.
  (evil-collection-key-blacklist '("SPC" "S-SPC"))

  ;; Org-mode derives from outline-mode; disable the outline bindings to prevent
  ;; conflicts.
  (evil-collection-outline-enable-in-minor-mode-p nil)

  ;; Be a bit smarter about the evil-collection load sequence; in particular,
  ;; set up bindings in hooks first time we activate a major-mode. This makes
  ;; key binding setup more performant and more predictable.
  :init
  (with-eval-after-load 'evil
    (require '+evil-collection)
    (+evil-collection-defer-install-to-mode-activation))
  :config
  (+evil-collection-init 'comint)

  ;; Fix leader keybindings that get clobbered by evil-collection.

  (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
    (general-define-key :keymaps (append evil-collection-magit-maps
                                         evil-collection-magit-section-maps)
                        :states '(normal)
                        "SPC" #'+leader-key)))


;; Evil-surround makes the S key work as an operator to surround an
;; object with, e.g., matched parentheses.
(use-package evil-surround :ensure t
  :hook ((text-mode-hook prog-mode-hook) . evil-surround-mode)
  ;; Use lowercase 's' for surround instead of 'S'.
  :general-config (:states '(visual) :keymaps 'evil-surround-mode-map "s" #'evil-surround-region)
  :custom
  (evil-surround-pairs-alist '((?\( . ("(" . ")"))
                               (?\) . ("(" . ")"))
                               (?\[ . ("[" . "]"))
                               (?\] . ("[" . "]"))
                               (?\{ . ("{" . "}"))
                               (?\} . ("{" . "}"))
                               (?# . ("#{" . "}"))
                               (?> . ("<" . ">"))
                               (?f . evil-surround-function)
                               (?t . evil-surround-read-tag)
                               (?< . evil-surround-read-tag)))

  :config
  (add-hook! 'emacs-lisp-mode-hook
    (make-local-variable 'evil-surround-pairs-alist)
    (alist-set! evil-surround-pairs-alist ?` '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?' '("`" . "'"))
    (alist-set! evil-surround-pairs-alist ?f #'evil-surround-prefix-function)))


;; Custom TTY cursor shape changer for evil states
(use-package evil-tty-cursor
  :after evil
  :demand t
  :config
  (global-evil-tty-cursor-mode +1))


;; Evil-compatible multiple cursors.
(use-package evil-multiedit :ensure t
  :after evil
  :demand t
  :config
  (evil-multiedit-default-keybinds)

  :init
  (defun +multiedit ()
    (interactive)
    (evil-normal-state)
    (unless (eolp)
      (forward-char -1))
    (evil-multiedit-match-all))

  :config
  (add-hook '+escape-hook
            (defun +evil-multiedit-escape-exit-h ()
              (when evil-multiedit-mode
                (evil-multiedit-abort)
                t)))

  :general
  (:states 'visual
           "v" (general-predicate-dispatch #'evil-multiedit-match-all
                 (equal last-command 'evil-visual-char) #'+multiedit))

  :general-config
  (:keymaps 'evil-multiedit-mode-map
   :states 'normal
   "Y" (defun +evil-multiedit-copy ()
         (interactive)
         (when-let* ((str (iedit-current-occurrence-string)))
           (kill-new str)
           (message "Copied to kill ring")))
   "<tab>" #'iedit-toggle-selection
   "TAB" #'iedit-toggle-selection
   "n" #'evil-multiedit-next
   "N" #'evil-multiedit-prev
   "S" #'evil-multiedit--change-line))


;; Show an indication in the modeline of how many evil-search hits are in the
;; buffer, and which one point last moved to.
(use-package evil-anzu :ensure t
  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))


(provide 'init-evil)

;;; init-evil.el ends here
