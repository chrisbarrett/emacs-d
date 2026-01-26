;;; evil/init.el --- Evil modal editing initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil modal editing with Vim emulation and ecosystem packages.

;;; Code:

(require '+corelib)

(autoload '+leader-key "init-leader")
(autoload 'general-define-key "general")

;;;; Core Evil setup

(use-package evil  :demand t
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
  ;; Cursor customisation
  (setq evil-normal-state-cursor 'box)
  (setq evil-emacs-state-cursor  'hollow)
  (setq evil-insert-state-cursor 'bar)
  (setq evil-visual-state-cursor 'hollow)

  ;; Initial states for modes
  (evil-set-initial-state 'beads-issue-mode 'insert)

  ;; Keybindings for beads-issue-mode
  (general-def :states '(insert normal) :keymaps 'beads-issue-mode-map
    "M-p" #'beads-issue-prev-message
    "M-n" #'beads-issue-next-message)

  (general-def :states '(normal) :keymaps 'beads-process-log-mode-map
    "q" #'bury-buffer)

  ;; Keep shift-width in sync if mode changes
  (setq-hook! 'after-change-major-mode
    evil-shift-width tab-width)

  ;; Use more natural Emacs/readline keybindings in ex
  (general-def :keymaps '(evil-ex-completion-map evil-ex-search-keymap)
    "C-a" #'evil-beginning-of-line
    "C-b" #'evil-backward-char
    "C-f" #'evil-forward-char)

  ;; Define newline behaviour.
  ;; `comment-indent-new-line' is a nicer default--it inserts comment delimiters
  ;; for you when you do a newline in a comment. However, it breaks
  ;; electric-pair's special newline padding functionality, so only call it if
  ;; we're actually on a comment.
  (general-def :states 'insert :keymaps '(prog-mode-map text-mode-map)
    "RET" (general-predicate-dispatch #'newline-and-indent
            (nth 4 (syntax-ppss))
            #'comment-indent-new-line))

  ;; Teach "J" (evil-join) to delete comment delimiters as needed
  (define-advice evil-join (:around (fn beg end) join-comments)
    (if-let* (((not (= (line-end-position) (point-max))))
              (cend (save-excursion (goto-char end) (line-end-position)))
              (cbeg (save-excursion
                      (goto-char beg)
                      (and (+point-in-comment-p
                            (save-excursion
                              (goto-char (line-beginning-position 2))
                              (skip-syntax-forward " \t")
                              (point)))
                           (or (comment-search-backward (line-beginning-position) t)
                               (comment-search-forward  (line-end-position) t)
                               (and (+point-in-comment-p beg)
                                    (stringp comment-continue)
                                    (or (search-forward comment-continue (line-end-position) t)
                                        beg)))))))
        (let* ((count (count-lines beg end))
               (count (if (> count 1) (1- count) count))
               (fixup-mark (make-marker)))
          (uncomment-region (line-beginning-position 2)
                            (save-excursion
                              (goto-char cend)
                              (line-end-position 0)))
          (unwind-protect
              (dotimes (_ count)
                (join-line 1)
                (save-match-data
                  (when (or (and comment-continue
                                 (not (string-empty-p comment-continue))
                                 (looking-at (concat "\\(\\s-*" (regexp-quote comment-continue) "\\) ")))
                            (and comment-start-skip
                                 (not (string-empty-p comment-start-skip))
                                 (looking-at (concat "\\(\\s-*" comment-start-skip "\\)"))))
                    (replace-match "" t nil nil 1)
                    (just-one-space))))
            (set-marker fixup-mark nil)))
      (funcall fn beg end)))

  ;; Teach evil-ret to open links at point
  (define-advice evil-ret (:before-until (&rest _) open-url)
    (when-let* ((url (thing-at-point 'url)))
      (browse-url url)
      t))

  (evil-mode +1)

  ;; Set up escape handling
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
            "C-w"    #'+delete-backward-word-no-kill)

  :general
  ("C-x SPC" '+insert-char))

;;;; Escape key setup

(global-set-key [remap keyboard-quit] #'+escape)
(global-set-key [remap abort-recursive-edit] #'+escape)
(with-eval-after-load 'evil
  (general-define-key :states 'normal [escape] #'+escape))
(general-define-key :keymaps +default-minibuffer-maps [escape] #'+escape)

(with-eval-after-load 'eldoc
  (eldoc-add-command '+escape))

;;;; Visual undo tree

(use-package vundo
  :general ("C-x u" #'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;;;; Evil-collection: community-managed keybindings

(use-package evil-collection  :custom
  (evil-collection-key-blacklist '("SPC" "S-SPC"))
  (evil-collection-outline-enable-in-minor-mode-p nil)
  (forge-add-default-bindings nil)

  :init
  (with-eval-after-load 'evil
    (+evil-collection-defer-install-to-mode-activation)
    (+evil-collection-init 'comint))

  :config
  (with-eval-after-load 'evil-collection-magit
    (define-advice evil-collection-magit-init (:after (&rest _) bind-leader)
      (general-define-key :keymaps (append evil-collection-magit-maps
                                           evil-collection-magit-section-maps)
                          :states '(normal)
                          "SPC" #'+leader-key))))

;;;; Evil-surround: surround text objects

(use-package evil-surround  :hook ((text-mode-hook prog-mode-hook conf-mode-hook) . evil-surround-mode)
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

;;;; Evil TTY cursor shapes

(use-package evil-tty-cursor
  :after evil
  :demand t
  :commands (global-evil-tty-cursor-mode)
  :config
  (global-evil-tty-cursor-mode +1))

;;;; Evil-multiedit: multiple cursors

(use-package evil-multiedit  :after evil
  :demand t
  :config
  (evil-multiedit-default-keybinds)

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
   "Y" #'+evil-multiedit-copy
   "<tab>" #'iedit-toggle-selection
   "TAB" #'iedit-toggle-selection
   "n" #'evil-multiedit-next
   "N" #'evil-multiedit-prev
   "S" #'evil-multiedit--change-line))

;;;; Evil-anzu: search hit count in modeline

(use-package evil-anzu  :after-call evil-ex-start-search evil-ex-start-word-search evil-ex-search-activate-highlight
  :config (global-anzu-mode +1))

;;;; Input handling

;; Insert non-breaking space on C-c SPC
(keymap-global-set "C-c SPC" #'+insert-nbsp)

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
                (key-binding (vconcat (if (= 0 (length keys)) [] (cl-subseq keys 0 -1))
                                      key)
                             nil t)))
         key fallback))))

;;;; Paragraph parsing

(use-package paragraphs
  :custom
  (sentence-end-double-space nil))

;;;; String inflection

(use-package string-inflection  :general (:states '(normal insert) "M--" #'string-inflection-all-cycle))

;;;; Comment handling

(use-package newcomment
  :custom
  (comment-empty-lines t)
  (comment-multi-line t)
  (comment-style 'extra-line)
  :config
  (setq-default comment-column 0))

;;;; Electric pairs

(use-package elec-pair
  :after-call +first-file-hook +first-buffer-hook
  :init
  (electric-pair-mode +1))

;;;; Structured editing with puni

(use-package puni  :after-call +first-file-hook +first-buffer-hook
  :hook (text-mode-hook prog-mode-hook conf-mode-hook)
  :general-config
  (:keymaps 'puni-mode-map :states 'insert "C-w" #'puni-backward-kill-word)
  (:keymaps 'puni-mode-map :states '(visual) "C-k" #'puni-kill-active-region)
  (:keymaps 'puni-mode-map :states '(insert normal emacs)
            "C-k" #'+kill-line
            "M-(" #'puni-wrap-round "M-)" #'puni-wrap-round
            "M-]" #'puni-wrap-square
            "M-{" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-previous))
            "M-}" (general-predicate-dispatch #'puni-wrap-curly
                    ((and (fboundp 'tempel--active-p)
                          (tempel--active-p nil (current-buffer)))
                     #'tempel-next))))

;;;; Navigation in prog-mode

(use-package lisp
  :general
  (:keymaps 'prog-mode-map :states 'normal "(" 'backward-sexp ")" 'forward-sexp))

(provide 'evil-init)

;;; evil/init.el ends here
