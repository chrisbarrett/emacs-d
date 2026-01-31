;;; evil/lib.el --- Evil library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for evil modal editing module.

;;; Code:

(require '+corelib)

;;;; Escape handling

(defvar +escape-hook nil
  "Hook functions run until success when ESC is pressed.")

;;;###autoload
(defun +escape (&optional interactive)
  "Quit things, abort things, and finish things.
Runs `+escape-hook'.

INTERACTIVE is set when called interactively."
  (interactive (list 'interactive))
  (let ((inhibit-quit t)
        (in-minibuffer? (minibuffer-window-active-p (minibuffer-window))))
    (cond
     (in-minibuffer?
      (when interactive (setq this-command 'abort-recursive-edit))
      (abort-recursive-edit))

     ;; Run all escape hooks. If any returns non-nil, then stop there.
     ((run-hook-with-args-until-success '+escape-hook))

     ;; Don't abort keyboard macros.
     ((or defining-kbd-macro executing-kbd-macro))

     ;; Fall back to keyboard-quit.
     (t
      (unwind-protect (keyboard-quit)
        (when interactive
          (setq this-command 'keyboard-quit)))))))

;;;; Evil-collection deferred loading

(defvar evil-collection-mode-list
  `(2048-game
    ag
    alchemist
    anaconda-mode
    apropos
    arc-mode
    atomic-chrome
    auto-package-update
    beginend
    bluetooth
    bm
    bookmark
    (buff-menu "buff-menu")
    bufler
    calc
    calendar
    cider
    citre
    cmake-mode
    color-rg
    comint
    company
    compile
    consult
    corfu
    crdt
    (csv "csv-mode")
    (custom cus-edit)
    cus-theme
    dape
    dashboard
    daemons
    deadgrep
    debbugs
    debug
    devdocs
    dictionary
    diff-hl
    diff-mode
    difftastic
    dired
    dired-sidebar
    disk-usage
    distel
    doc-view
    docker
    eat
    ebib
    ebuku
    edbi
    edebug
    ediff
    eglot
    elpaca
    ement
    explain-pause-mode
    eldoc
    elfeed
    elisp-mode
    elisp-refs
    elisp-slime-nav
    embark
    emms
    ,@(when (>= emacs-major-version 29) '(emoji))
    epa
    ert
    eshell
    eval-sexp-fu
    evil-mc
    eww
    fanyi
    finder
    flycheck
    flymake
    forge
    free-keys
    geiser
    ggtags
    git-timemachine
    gited
    gnus
    go-mode
    gptel
    grep
    guix
    hackernews
    helm
    help
    helpful
    hg-histedit
    hideshow
    hungry-delete
    hyrolo
    ibuffer
    (image image-mode)
    image-dired
    image+
    imenu
    imenu-list
    (indent "indent")
    indium
    info
    ivy
    js2-mode
    ,@(when (>= emacs-major-version 30) '(kmacro))
    leetcode
    lispy
    lms
    log-edit
    log-view
    lsp-ui-imenu
    lua-mode
    kotlin-mode
    macrostep
    man
    (magit magit-submodule)
    magit-repos
    magit-section
    magit-todos
    markdown-mode
    minesweeper
    monky
    mpc
    mpdel
    mpdired
    mu4e
    mu4e-conversation
    neotree
    newsticker
    notmuch
    nov
    omnisharp
    org
    org-present
    org-roam
    osx-dictionary
    outline
    ovpn-mode
    p4
    (package-menu package)
    pass
    (pdf pdf-view)
    popup
    proced
    (process-menu simple)
    prodigy
    profiler
    p-search
    python
    quickrun
    racer
    racket-describe
    reader
    realgud
    reftex
    replace
    restclient
    rg
    ripgrep
    rjsx-mode
    robe
    rtags
    ruby-mode
    scheme
    scroll-lock
    selectrum
    sh-script
    ,@(when (>= emacs-major-version 28) '(shortdoc))
    simple
    simple-mpc
    slime
    sly
    smerge-mode
    snake
    so-long
    speedbar
    ,@(when (>= emacs-major-version 27) '(tab-bar))
    tablist
    tabulated-list
    tar-mode
    telega
    (term term ansi-term multi-term)
    tetris
    ,@(when (>= emacs-major-version 27) '(thread))
    tide
    timer-list
    transmission
    trashed
    tuareg
    typescript-mode
    ultra-scroll
    vc-annotate
    vc-dir
    vc-git
    vdiff
    vertico
    view
    vlf
    vterm
    vundo
    w3m
    wdired
    wgrep
    which-key
    with-editor
    woman
    xref
    xwidget
    yaml-mode
    youtube-dl
    zmusic
    (ztree ztree-diff ztree-dir))
  "List of modes supported by evil-collection.

Elements are either target mode symbols or lists which `car' is the mode
symbol and `cdr' the packages to register.")

;;;###autoload
(defvar +evil-collection-disabled-list
  '(anaconda-mode
    buff-menu
    calc
    comint
    company
    custom
    eldoc
    elisp-mode
    ert
    free-keys
    helm
    help
    image
    indent
    kmacro
    kotlin-mode
    lispy
    outline
    replace
    shortdoc
    simple
    slime
    tab-bar)
  "List of modes to exclude from evil-collection initialization.")

;; We handle loading evil-collection ourselves
(defvar evil-collection--supported-modes nil)

;;;###autoload
(defun +evil-collection-init (module &optional disabled-list)
  "Initialise evil-collection-$module MODULE.

Unlike `evil-collection-init', this respects `+evil-collection-disabled-list',
and complains if a module is loaded too early (during startup).

DISABLED-LIST is used to conditionally disable specific modules."
  (unless (memq (or (car-safe module) module) disabled-list)
    (+log "editor:evil: loading evil-collection-%s %s"
          (or (car-safe module) module)
          (if after-init-time "" "(too early!)"))
    (with-demoted-errors "evil-collection error: %s"
      (evil-collection-init (list module)))))

;;;###autoload
(defun +evil-collection-defer-install-to-mode-activation ()
  "Set up deferred loading of evil-collection modules."
  (add-transient-hook! 'help-mode (+evil-collection-init 'help))
  (add-transient-hook! 'Buffer-menu-mode (+evil-collection-init '(buff-menu "buff-menu")))
  (add-transient-hook! 'calc-mode (+evil-collection-init 'calc))
  (add-transient-hook! 'image-mode (+evil-collection-init 'image))
  (add-transient-hook! 'emacs-lisp-mode (+evil-collection-init 'elisp-mode))
  (add-transient-hook! 'occur-mode (+evil-collection-init 'replace))
  (add-transient-hook! 'indent-rigidly (+evil-collection-init '(indent "indent")))
  (add-transient-hook! 'kmacro-menu-mode (+evil-collection-init 'kmacro))
  (add-transient-hook! 'process-menu-mode (+evil-collection-init '(process-menu simple)))
  (add-transient-hook! 'shortdoc-mode (+evil-collection-init 'shortdoc))
  (add-transient-hook! 'tabulated-list-mode (+evil-collection-init 'tabulated-list))
  (add-transient-hook! 'tab-bar-mode (+evil-collection-init 'tab-bar))

  (dolist (mode evil-collection-mode-list)
    (dolist (req (or (cdr-safe mode) (list mode)))
      (with-eval-after-load req
        (+evil-collection-init mode +evil-collection-disabled-list)))))

;;;; Minibuffer functions

;;;###autoload
(defun +delete-backward-word-no-kill (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring.
ARG is the number of words to delete."
  (interactive "p")
  (let ((kill-ring nil) (kill-ring-yank-pointer nil))
    (ignore-errors (backward-kill-word arg))))

;;;; Insert commands

;;;###autoload
(defun +insert-char ()
  "Insert a character at point."
  (interactive)
  (require 'evil)
  (evil-insert-state)
  (call-interactively
   (if (equal system-type 'darwin)
       #'ns-do-show-character-palette
     #'insert-char)))

;;;###autoload
(defun +insert-nbsp ()
  "Insert a non-breaking space at point."
  (interactive)
  (insert-char #x00A0))

;;;; Multiedit functions

;;;###autoload
(defun +multiedit ()
  "Start multiedit on all matches of current selection."
  (interactive)
  (require 'evil)
  (require 'evil-multiedit)
  (evil-normal-state)
  (unless (eolp)
    (forward-char -1))
  (evil-multiedit-match-all))

;;;###autoload
(defun +evil-multiedit-copy ()
  "Copy current multiedit occurrence to kill ring."
  (interactive)
  (when-let* ((str (iedit-current-occurrence-string)))
    (kill-new str)
    (message "Copied to kill ring")))

;;;; Kill functions

;;;###autoload
(defun +kill-line ()
  "Kill line with puni, then format."
  (interactive)
  (require 'puni)
  (puni-kill-line)
  (when-let* ((cmd (and (derived-mode-p 'prog-mode)
                        indent-line-function)))
    (funcall cmd)))



;;; lib.el ends here
