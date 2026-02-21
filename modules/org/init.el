;;; org-init.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Org-mode core configuration with visual enhancements, evil integration,
;; and custom workflows.

;;; Code:

(require '+autoloads)

(require '+corelib)

(cl-eval-when (compile)
  (require 'evil)
  (require 'general)
  (require 'org)
  (require 'org-archive)
  (require 'org-capture)
  (require 'org-clock)
  (require 'org-duration)
  (require 'org-indent)
  (require 'org-src)
  (require 'ol)
  (require 'ox))


;;; Core org-mode package

(use-package org
  :hook ((org-mode-hook . abbrev-mode)
         (org-mode-hook . auto-fill-mode))

  :general ("C-c a" #'org-agenda)

  :general (:keymaps 'org-mode-map "TAB" 'org-cycle)

  :custom
  (abbrev-file-name (file-name-concat org-directory "abbrev.el"))
  (org-babel-load-languages '((emacs-lisp . t)
                              (C . t)
                              (calc . t)
                              (shell . t)))

  :config

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

  ;; Always re-hide drawers when cycling fold levels
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

  ;; Remap mode definitions for tree-sitter modes.
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
            '("yml" . yaml-ts))


  ;;; Babel optimization

  ;; Inhibit expensive local-var hooks (e.g., LSP) in temporary babel edit
  ;; buffers. These buffers are ephemeral and don't need full tooling setup.

  (define-advice org-src--edit-element (:around (fn datum name &optional initialize &rest args) +org-inhibit-local-var-hooks-a)
    "Prevent potentially expensive mode hooks in `org-babel-do-in-edit-buffer' ops."
    (apply fn
           datum
           name
           (if (and (eq org-src-window-setup 'switch-invisibly) (functionp initialize))
               (lambda ()
                 (+with-inhibit-local-var-hooks
                   (funcall initialize)))
             initialize)
           args))


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

  (add-transient-hook! 'org-mode-local-vars-hook #'org-clock-persistence-insinuate)

  (setq org-clock-persist t)


  ;;; Archive

  ;; org-mode's glacier storage tier.

  (setq org-archive-subtree-add-inherited-tags t)
  (setq org-archive-location (file-name-concat org-directory "archive.org::datetree/"))


  ;;; Export settings

  ;; Disable table of contents by default for all exports
  (setq org-export-with-toc nil)

  ;; Disable entity conversion - use Unicode characters directly instead of HTML entities
  ;; This affects both regular markdown (ox-md) and GitHub-flavored markdown (ox-gfm)
  (setq org-export-with-entities nil)


  ;;; Local leader keybindings

  (+local-leader-set-key 'org-mode-map
    "A" #'org-archive-subtree
    "I" #'org-id-get-create
    "y" #'org-copy-subtree
    "x" #'org-cut-subtree
    "p" #'org-paste-subtree
    "t" #'org-show-todo-tree)


  ;;; Normal state keybindings

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


  ;;; Normal + Insert state keybindings

  (general-def :keymaps 'org-mode-map :states '(normal insert)
    "C-c f" #'org-footnote-new
    "M-+" #'org-table-insert-column
    "M--" #'org-table-delete-column
    "C-c C-." #'org-time-stamp-inactive
    "C-c ." #'org-time-stamp

    "C-c RET" (general-predicate-dispatch #'org-insert-todo-heading
                (org-at-table-p) #'org-table-hline-and-move)

    "C-c C-k" #'+org-cut-subtree-or-cancel-note)


  ;;; Evil integration

  ;; Automatically enter insert state when inserting new headings, logbook notes
  ;; or when using `org-capture'.

  (with-eval-after-load 'evil
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
  (add-hook 'org-metareturn-hook #'+org-metareturn-insert-heading-insert-state-h))


;;; Link configuration (after ol loads)

(with-eval-after-load 'ol
  ;; Highlight broken file links.
  (org-link-set-parameters
   "file" :face (lambda (path)
                  (if (or (file-remote-p path)
                          (file-exists-p path))
                      'org-link
                    '(warning org-link))))

  (org-link-set-parameters
   "crate"
   :follow (lambda (name)
             (browse-url (format "https://docs.rs/%s/latest/%s" name name)))
   :export (lambda (path desc format)
             (let ((crate-name path)
                   (desc (or desc (concat "Crate " path))))
               (pcase format
                 (`html (format "<a href=\"https://crates.io/crates/%s\">%s</a>" crate-name desc))
                 (`latex (format "\\href{https://crates.io/crates/%s}{%s}" crate-name desc))
                 (_ desc)))))

  (org-link-set-parameters
   "github"
   :follow (lambda (path)
             (browse-url (format "https://github.com/%s" path)))
   :export (lambda (path desc format)
             (let ((desc (or desc (concat "GitHub: " path))))
               (pcase format
                 (`html (format "<a href=\"https://github.com/%s\">%s</a>" path desc))
                 (`latex (format "\\href{https://github.com/%s}{%s}" path desc))
                 (_ desc)))))

  (autoload 'rfc-mode--document-buffer "rfc-mode")

  (org-link-set-parameters
   "RFC"
   :follow (lambda (number)
             (require 'rfc-mode)
             (pop-to-buffer (rfc-mode--document-buffer (string-to-number number))))
   :export (lambda (path desc format)
             (let ((rfc-num path)
                   (desc (or desc (concat "RFC " path))))
               (pcase format
                 (`html (format "<a href=\"https://www.rfc-editor.org/rfc/rfc%s.html\">%s</a>" rfc-num desc))
                 (`latex (format "\\href{https://www.rfc-editor.org/rfc/rfc%s.html}{%s}" rfc-num desc))
                 (_ desc)))))

  ;; Use a distinct face for ID (org-roam) links
  (org-link-set-parameters "id" :face '+org-id-link)

  ;; Make RET follow ID links in the same window.
  (define-advice org-id-open (:around (orig-fun id &optional argument) follow-links-same-window)
    (let ((org-link-frame-setup '((file . find-file))))
      (funcall orig-fun id argument)))

  ;; Load extra link types provided by features
  (require 'ol-man))


;;; Evil keybindings for org-mode

(use-package evil-org
  :hook (org-mode-hook . evil-org-mode)
  :custom
  (evil-org-key-theme '(todo navigation insert textobjects additional calendar))
  :init
  (use-package evil-org-agenda
    :after org-agenda
    :demand t
    :config
    (evil-org-agenda-set-keys)
    (evil-define-key 'motion org-agenda-mode-map
      (kbd "v") #'org-agenda-view-mode-dispatch
      (kbd "SPC") nil
      (kbd "/") #'org-agenda-filter)))


;;; Group items in the agenda

(use-package org-super-agenda
  :after org-agenda
  :demand t
  :custom
  (org-super-agenda-hide-empty-groups t)
  :config
  (org-super-agenda-mode +1)
  ;; Clear the keymap to ensure the regular evil keybindings for org-agenda work.
  (setq org-super-agenda-header-map (make-sparse-keymap)))


;;; Edit comments or strings in dedicated buffer

(use-package separedit
  :commands (separedit-commit)
  :custom
  (separedit-default-mode 'gfm-mode))


;;; Visual enhancements

(use-package org-modern
  :after org
  :demand t
  :custom
  (org-modern-table nil) ; looks terrible in TTY
  (org-modern-hide-stars nil)
  (org-modern-fold-stars
   '(("▶" . "▼") ("▹" . "▿") ("▸" . "▾") ("⯈" . "⯆")))
  (org-modern-block-name
   `(("src" . ("" "◌"))
     ("quote" . ("" "◌"))
     ("example" . ("" "◌"))))
  :config
  (global-org-modern-mode +1)
  (custom-theme-set-faces 'user
                          '(org-todo ((t (:bold t :inverse-video t))))
                          ;; Based on `eldoc-highlight-function-argument'
                          '(org-modern-date-active ((((background dark))
                                                     (:foreground "#dfaf7a"
                                                      :background "#381d0f"
                                                      :weight light
                                                      :inherit org-modern-label))
                                                    (((background light))
                                                     (:foreground "#884900"
                                                      :background "#f8f0d0"
                                                      :weight light
                                                      :inherit org-modern-label)))))

  (let ((custom-todos
         '(("WAIT" warning org-todo)
           ("PROJECT" font-lock-keyword-face org-todo))))
    (setq org-modern-todo-faces custom-todos)
    (setq org-todo-keyword-faces
          (seq-map (apply-partially #'take 2) custom-todos))))


;;; Create org-mode links from clipboard URLs

(use-package org-cliplink
  :general (:keymaps 'org-mode-map "C-c l" #'org-cliplink))


;;; Calendar widget

(use-package calendar
  :custom
  (calendar-mode-line-format nil)
  (calendar-date-style 'iso)
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1))


;;; GitHub-flavored markdown exporter

(use-package ox-gfm
  :after ox
  :demand t)



;;; init.el ends here
