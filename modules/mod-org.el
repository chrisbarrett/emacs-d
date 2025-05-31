;;; mod-org.el --- Configuration for org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'org)
(require 'general)

(cl-eval-when (compile)
  (require 'evil)
  (require 'org-archive)
  (require 'org-capture)
  (require 'org-clock)
  (require 'org-duration)
  (require 'org-habit)
  (require 'org-indent)
  (require 'org-src))



(+local-leader-set-key 'org-mode-map
  "A" #'org-archive-subtree
  "I" #'org-id-get-create
  "y" #'org-copy-subtree
  "x" #'org-cut-subtree
  "p" #'org-paste-subtree
  "t" #'org-show-todo-tree)

(general-def :keymaps 'org-mode-map :states 'normal
  "RET" 'org-open-at-point
  "M-p" #'org-metaup
  "M-n" #'org-metadown
  "C-c c" #'org-columns
  "C-c d" #'org-dynamic-block-insert-dblock
  "C-c n" 'org-next-link
  "C-c p" 'org-previous-link
  "C-c o" 'org-table-toggle-coordinate-overlays
  "SPC n s" #'org-narrow-to-subtree)

(general-def :keymaps 'org-mode-map :states '(normal insert)
  "C-c f" #'org-footnote-new
  "M-+" #'org-table-insert-column
  "M--" #'org-table-delete-column
  "C-c C-." #'org-time-stamp-inactive
  "C-c ." #'org-time-stamp

  "C-c RET" (general-predicate-dispatch #'org-insert-todo-heading
              (org-at-table-p) #'org-table-hline-and-move)

  "C-c C-k" (defun +org-cut-subtree-or-cancel-note ()
              (interactive)
              (cond (org-finish-function
                     (let ((org-note-abort t)) (funcall org-finish-function)))
                    ((bound-and-true-p org-capture-mode)
                     (org-capture-kill))
                    (t
                     (org-cut-subtree)))))


;;; Cosmetic & visuals

(setq org-list-indent-offset 1)
(setq org-cycle-separator-lines 0)
(setq org-ellipsis " …")
(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-startup-folded nil)
(setq org-startup-indented t)
(setq org-startup-shrink-all-tables t)
(setq org-startup-with-inline-images t)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-fontify-whole-heading-line t)

(setq org-indent-indentation-per-level 3)

(setq org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . success)))

(setq org-tags-column 0)
(setq org-auto-align-tags nil)
(setq org-fold-catch-invisible-edits 'show-and-error)

;; `org-hidden-keywords' does not hide the spaces between the keyword and the
;; value. I just do the hiding myself.
(font-lock-add-keywords 'org-mode
                        `((,(rx bol "#+title:" (+ space)) 0
                           '(face nil invisible t) prepend)))

;; Don't show secondary selection when running `org-show-todo-tree'.
(advice-add #'org-highlight-new-match :override #'ignore)

;; Prevent flickering when org-indent is enabled.
(setq-hook! 'org-mode-hook show-paren-mode nil)

;; Increase padding for readability.
(setq-hook! '(org-mode-hook org-agenda-mode-hook) line-spacing 0.1)

(custom-theme-set-faces 'user
                        '(org-footnote ((t (:underline nil))))
                        '(org-document-title ((t (:height 2.0)))))

;; Always re-hide draws cycling fold levels

(add-hook! 'org-cycle-hook
  (org-cycle-hide-drawers 'all))


;;; TODOs, checkboxes, stats, properties.

(setq org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c@)")
                          (type "PROJECT(p)" "|" "DONE(d!)")))
(setq org-use-fast-todo-selection 'expert)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-hierarchical-todo-statistics nil)
(setq org-use-property-inheritance t)
(setq org-log-into-drawer t)
(setq org-duration-format 'h:mm)


;;; Interactive behaviour

(setq org-imenu-depth 5)
(setq org-bookmark-names-plist nil)
(setq org-M-RET-may-split-line nil)
(setq org-footnote-auto-adjust t)
(setq org-insert-heading-respect-content t)
(setq org-loop-over-headlines-in-active-region 'start-level)
(setq org-return-follows-link t)
(setq org-track-ordered-property-with-tag t)

;; Ensure we use dired rather than the Finder on macOS.

(when (equal system-type 'darwin)
  (add-to-list 'org-file-apps '(directory . emacs)))

;; Automatically enter insert state when inserting new headings, logbook notes
;; or when using `org-capture'.

(with-eval-after-load 'evil
  (defun +ad-org-enter-evil-insert-state (&rest _)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil))
      (evil-insert-state)))

  (dolist (cmd '(org-insert-heading
                 org-insert-heading-respect-content
                 org-insert-todo-heading-respect-content
                 org-insert-todo-heading))
    (advice-add cmd :after #'+ad-org-enter-evil-insert-state))

  (define-advice org-capture (:after (&rest _) insert-state)
    (when (and (bound-and-true-p evil-mode)
               (called-interactively-p nil)
               (bound-and-true-p org-capture-mode))
      (evil-insert-state)))

  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))

;; Prefer inserting headings with M-RET

(add-hook 'org-metareturn-hook
          (defun +org-metareturn-insert-heading-insert-state-h ()
            (when (org-in-item-p)
              (org-insert-heading current-prefix-arg)
              (when (bound-and-true-p evil-mode)
                (evil-append-line 1))
              t)))


;;; Hyperlinks

(setq org-link-abbrev-alist
      '(("github"      . "https://github.com/%s")
        ("youtube"     . "https://youtube.com/watch?v=%s")
        ("google"      . "https://google.com/search?q=")
        ("wikipedia"   . "https://en.wikipedia.org/wiki/%s")))


;;; Babel & src blocks

(setq org-edit-src-content-indentation 0)
(setq org-src-preserve-indentation nil)
(setq org-src-window-setup 'plain)
(setq org-confirm-babel-evaluate nil)

(defvar org-babel-default-header-args:emacs-lisp
  '((:lexical . "yes")))

(defvar org-babel-default-header-args:C
  `((:includes
     "<stdio.h>"
     "<stdlib.h>"
     "<stdint.h>"
     "<assert.h>"
     "<stdalign.h>"
     "<string.h>"
     "<fcntl.h>"
     "<errno.h>")))

(defvar org-babel-python-command "python3")

;; Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation.

(define-advice org-return (:after (&optional indent _arg _interactive) emulate-major-mode-indent)
  (when (and indent org-src-tab-acts-natively (org-in-src-block-p t))
    (save-window-excursion
      (org-babel-do-in-edit-buffer
       (call-interactively #'indent-for-tab-command)))))

;; TODO: Remap mode definitions so I don't have to maintain this
;; myself...
(pushnew! org-src-lang-modes
          '("cs" . csharp-ts)
          '("csharp" . csharp-ts)
          '("docker" . dockerfile-ts)
          '("dockerfile" . dockerfile-ts)
          '("elixir" . elixir-ts)
          '("json" . json-ts)
          '("md" . markdown)
          '("nix" . nix-ts)
          '("rs" . rust-ts)
          '("rust" . rust-ts)
          '("sh" . bash-ts)
          '("ts" . typescript-ts)
          '("typescript" . typescript-ts)
          '("yaml" . yaml-ts)
          '("yml" . yaml-ts)
          )


;;; Refile

;; Move org-mode headings and their contents around in a structured way,
;; both within the current file and to others.

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; When refiling from org-capture, Emacs prompts to kill the underlying,
;; modified buffer. This fixes that.

(add-hook 'org-after-refile-insert-hook #'save-buffer)


;;; Clocking

(add-transient-hook! 'org-mode-hook #'org-clock-persistence-insinuate)

(require '+clockreport) ; used in my org config

(setq org-clock-persist t)


;;; org-habit

;; Declare certain tasks in the agenda as 'habits'; these have a graph
;; displayed beside them to help visualise your consistency.

(setq org-habit-graph-column 72)
(setq org-habit-today-glyph ?▲)
(setq org-habit-completed-glyph ?✓)

(defvar +org-habit-graph-window-ratio 0.2
  "The ratio of the consistency graphs relative to the window width.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph.")

(defvar +org-habit-min-width 30
  "Hide the consistency graph if `org-habit-graph-column' is less than this.")

(add-hook 'org-agenda-mode-hook
          (defun +org-habit-resize-graph-h ()
            "Right align and resize the consistency graphs based on
`+org-habit-graph-window-ratio'"
            (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
                   (preceding-days-ratio (/ org-habit-preceding-days total-days))
                   (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
                   (preceding-days (floor (* graph-width preceding-days-ratio)))
                   (following-days (- graph-width preceding-days))
                   (graph-column (- (window-width) (+ preceding-days following-days)))
                   (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                              (- graph-column +org-habit-graph-padding)
                                            nil)))
              (setq-local org-habit-preceding-days preceding-days)
              (setq-local org-habit-following-days following-days)
              (setq-local org-habit-graph-column graph-column-adjusted))))


;;; Archive

;; org-mode's glacier storage tier.

(setq org-archive-subtree-add-inherited-tags t)
(setq org-archive-location (file-name-concat org-directory "archive.org::datetree/"))

(provide 'mod-org)

;;; mod-org.el ends here
