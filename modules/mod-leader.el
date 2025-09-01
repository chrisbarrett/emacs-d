;;; mod-leader.el --- Leader key -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'general)
(require '+window)
(require '+org-roam)
(require '+edit-cmds)
(require '+files)

(general-auto-unbind-keys)
(general-unbind :states '(normal motion) "SPC" "M-m")

(general-create-definer +define-leader-keys :states '(normal motion))

(+define-leader-keys :prefix "SPC" ;; root-level
  :prefix-command '+leader-key

  "SPC" '(consult-buffer :wk "buffers & files")
  "RET" '(claude-code-ide-menu :wk "LLM...")
  "{" '(other-frame-prefix :wk "in other frame...")
  "." '(other-window-prefix :wk "in other window...")
  "!" '(async-shell-command :wk "shell command")
  "|" '(rotate-layout :wk "rotate window layout")
  "-" '(window-toggle-side-windows :wk "side windows")
  ":" '(pp-eval-expression :wk "eval")
  ";" '(ielm :wk "REPL")
  "C" '(claude-code-ide-menu :wk "Claude Code")
  "d" (list (general-predicate-dispatch #'dired-jump
              current-prefix-arg #'dired
              ;; If already in dired, open elsewhere with completing-read.
              (derived-mode-p 'dired-mode) #'dired)
            :wk "dir editor")
  "i" '(consult-imenu :wk "imenu")
  "r" #'vertico-repeat
  "s" '(save-buffer :wk "save buf")
  "S" '(save-some-buffers :wk "save some bufs...")
  "u" '(universal-argument :wk "C-u")
  "x" '(execute-extended-command :wk "M-x")
  "K" 'man
  "T" (list (defun +goto-mode-template-file ()
              (interactive)
              (let* ((modes (nreverse (parent-mode-list major-mode)))
                     (mode (completing-read "Snippets table for mode: " modes nil t))
                     (filename (format "%s.eld" (string-remove-suffix "-mode" mode))))
                (find-file (file-name-concat user-emacs-directory "templates" filename))))
            :wk "edit templates...")


  "'" (general-predicate-dispatch #'separedit

        ;; Exit indirect edit session if active

        (bound-and-true-p edit-indirect--overlay) #'separedit-commit
        (bound-and-true-p org-src-mode) #'org-edit-src-exit

        ;; Otherwise, open indirect-edit buffer

        (and (derived-mode-p 'prog-mode)
             ;; Are we in a string or comment? See: `parse-partial-sexp'
             (or (nth 3 (syntax-ppss)) (nth 4 (syntax-ppss))))
        #'separedit

        (and (derived-mode-p 'prog-mode) (region-active-p)) #'separedit
        (equal (buffer-name) "*Edit Formulas*") #'org-table-fedit-finish
        (derived-mode-p 'org-mode) #'org-edit-special
        (and (derived-mode-p 'markdown-mode) (markdown-code-block-at-point-p)) 'markdown-edit-code-block)

  "/" '(consult-ripgrep :wk "search (rg)")
  "*" (list (defun +consult-ripgrep-symbol ()
              (interactive)
              (consult-ripgrep nil (format "%s" (symbol-at-point))))
            :wk "search (symbol)")

  "<tab>" (list (defun +swap-buffers ()
                  "Switch between the previous buffer and the current one."
                  (interactive)
                  (switch-to-buffer nil))
                :wk "swap bufs")

  "p"  '(nil :wk "project")
  "p" project-prefix-map
  "h"  '(nil :wk "help")
  "h" help-map
  "," '(nil :wk "structure")
  "a"  '(nil :wk "apps")
  "b"  '(nil :wk "buffers")
  "f"  '(nil :wk "files")
  "n"  '(nil :wk "narrowing")
  "c"  '(nil :wk "code/comments")
  "g"  '(nil :wk "git/goto")
  "C" '(claude-code-ide-send-prompt :wk "Tell claude-code...")
  "l" '(claude-code-ide-menu :wk "claude-code...")
  "o"  '(nil :wk "org")
  "e"  '(nil :wk "errors")
  "k" '(consult-yank-pop :wk "kill-ring")
  "t"  '(nil :wk "toggles")
  "w"  '(nil :wk "windows")
  "z" '(global-text-scale-adjust :wk "text scaling"))

(+define-leader-keys :prefix "SPC ," ;; structure
  "n" '(puni-forward-sexp :wk "forward-sexp")
  "p" '(puni-backward-sexp :wk "backward-sexp")
  "<" '(puni-backward-sexp-or-up-list :wk "backward-sexp-or-up-list")
  "c" '(puni-convolute :wk "convolute")
  "d" '(+forward-kill-sexp :wk "kill sexp forward")
  "D" '(+backward-kill-sexp :wk "kill sexp back")

  "k" '(puni-splice-killing-forward :wk "splice-killing-forward")
  "K" '(puni-splice-killing-backward :wk "splice-killing-backward")

  ;; TODO: define a killing-around variant.
  "s" '(puni-splice-killing-backward :wk "splice-killing-backward")

  "r" '(puni-raise :wk "raise")
  "b" '(puni-barf-forward :wk "barf-forward")
  "B" '(puni-barf-backward :wk "barf-backward")
  "m" '(puni-slurp-forward :wk "slurp-forward")
  "M" '(puni-slurp-backward :wk "slurp-backward")
  "t" '(puni-transpose :wk "transpose")
  "u" '(puni-splice :wk "splice")
  "x" '(puni-split :wk "split"))

(+define-leader-keys :prefix "SPC a" ;; apps
  "n" #'elfeed
  "w" #'eww
  "c" #'quick-calc
  "C" #'full-calc
  "e" #'eshell
  "s" #'eat
  "r" (general-predicate-dispatch 'profiler-start
        (and (featurep 'profiler) (profiler-running-p))
        (defun +profiler-stop-and-report (&optional continue-p)
          (interactive "P")
          (let ((ran-p (profiler-running-p)))

            (unless continue-p
              (profiler-stop))
            (profiler-report)
            (when ran-p
              (if continue-p
                  (message "Profiler still recording")
                (message "Profiler stopped"))))))
  "p"  '(nil :wk "elpaca"))

(+define-leader-keys :prefix "SPC a p" ;; elpaca
  "p" #'elpaca-manager
  "l" #'elpaca-log
  "i" #'elpaca-info
  "b" #'elpaca-browse
  "v" #'elpaca-visit)

(+define-leader-keys :prefix "SPC b" ;; buffers
  "b" '(bury-buffer :wk "bury")
  "d" '(bury-buffer :wk "bury")
  "D" '(kill-current-buffer :wk "kill")
  "l" '(bufler :wk "list")
  "n" '(next-buffer :wk "next")
  "s" '(bufler-switch-buffer :wk "switch...")
  "p" '(previous-buffer :wk "prev")
  "c" (list (general-predicate-dispatch #'clone-indirect-buffer
              (region-active-p) #'+clone-indirect-buffer-of-region)
            :wk "clone indirect"))

(+define-leader-keys :prefix "SPC c" ;; code/comments
  "m" '(xref-find-references :wk "find refs")
  "r" '(comment-dwim :wk "comment (dwim)")
  "d" '(eglot-find-typeDefinition :wk "find type def")
  "c" '(eglot-find-declaration :wk "find decl")
  "i" '(eglot-find-implementation :wk "find impl")
  "l" '(comment-line :wk "comment out"))

(+define-leader-keys :prefix "SPC e" ;; errors
  "l" '(consult-flymake :wk "error list")
  "e" '(first-error :wk "first error")
  "n" '(next-error :wk "next error")
  "p" '(previous-error :wk "prev error"))

(+define-leader-keys :prefix "SPC f" ;; files
  "f" '(find-file :wk "find")
  "g" '(magit-find-file :wk "find (in git rev...)")
  "F" '(find-file-other-window :wk "find (other window)")
  "s" '(save-buffer :wk "save")
  "R" '(rename-visited-file :wk "rename")
  "r" '(recentf :wk "recent")
  "w" '(write-file :wk "write copy")
  "o" '(+find-sibling-file :wk "other file")

  "D" (list (defun +delete-file-and-buffer ()
              (interactive)
              (let ((file (buffer-file-name)))
                (kill-buffer (current-buffer))
                (when file
                  (delete-file file))))
            :wk "delete file & buf")

  "y" (list (defun +copy-file-path ()
              (interactive)
              (if-let* ((file (buffer-file-name)))
                  (progn
                    (kill-new file)
                    (message "Copied to clipboard => %s" file))
                (user-error "Buffer is not visiting a file")))
            :wk "copy (full path)")

  "d" (list (defun +copy-file-directory ()
              (interactive)
              (let ((dir (or (-some->> (buffer-file-name) (file-name-directory))
                             default-directory)))
                (kill-new dir)
                (message "Copied to clipboard => %s" dir)))
            :wk "copy (dir)")

  "v" (list (defun +revisit-file ()
              (interactive)
              (if-let* ((file (buffer-file-name)))
                  (find-alternate-file file)
                (user-error "Buffer is not visiting a file")))
            :wk "reload"))

(+define-leader-keys :prefix "SPC g" ;; git/goto
  "b" '(magit-blame :wk "blame")
  "d" '(magit-diff-buffer-file :wk "buffer diff")
  "f" '(magit-file-dispatch :wk "file actions...")
  "g" '(magit-status :wk "status")
  "l" '(magit-log-buffer-file :wk "buffer log")
  "p" '(forge-browse-pullreq :wk "browse pullreq")
  "r" '(browse-at-remote :wk "open on GitHub")
  "t" '(git-timemachine-toggle :wk "file history")
  "y" '(browse-at-remote-kill :wk "copy GitHub link ")

  "?" (list (defun +goto-messages ()
              (interactive)
              (display-buffer "*Messages*"))
            :wk "messages")

  "e" (list (defun +goto-emacs-init-file ()
              (interactive)
              (when (bound-and-true-p beframe-mode)
                (project-switch-beframed user-emacs-directory))
              (find-file (file-name-concat user-emacs-directory "init.el")))
            :wk "init file")

  "s" (list (defun +goto-emacs-site-file ()
              (interactive)
              (project-find-file-in nil +site-files-directory
                                    (project-current nil user-emacs-directory)))
            :wk "site file...")

  "n" (list (defun +goto-nix-file ()
              (interactive)
              (project-find-file-in  "flake.nix" nil
                                     (project-current nil "~/.config/nix-configuration")))
            :wk "nix config file..."))

(+define-leader-keys :prefix "SPC n" ;; narrowing
  "f" '(narrow-to-defun :wk "defun")
  "r" '(narrow-to-region :wk "region")
  "w" #'widen)

(+define-leader-keys :prefix "SPC o" ;; org
  "n" (list (defun +org-goto-notes ()
              (interactive)
              (when (bound-and-true-p beframe-mode)
                (project-switch-beframed org-directory))
              (find-file org-default-notes-file))
            :wk "notes")
  "i" (list (defun +goto-org-roam-index ()
              (interactive)
              (when (bound-and-true-p beframe-mode)
                (project-switch-beframed org-directory))
              (find-file (file-name-concat org-roam-directory "notes/index.org")))
            :wk "roam index")
  "t" (list (defun +goto-org-todos ()
              (interactive)
              (when (bound-and-true-p beframe-mode)
                (project-switch-beframed org-directory))
              (find-file (file-name-concat org-roam-directory "todos.org")))
            :wk "todos")
  "a" (list (defun +org-agenda-dwim ()
              (interactive)
              (when (bound-and-true-p beframe-mode)
                (project-switch-beframed org-directory))
              (org-agenda nil "p"))
            :wk "agenda")
  "j" '(consult-org-agenda :wk "agenda file heading...")
  "g" '(org-capture-goto-last-stored :wk "goto captured")
  "v" '(org-tags-view :wk "search by tag")
  "k" #'org-capture
  "l" '(org-store-link :wk "store link")
  "f" '(+org-roam-node-find :wk "find (roam)")
  "s" '(org-roam-search :wk "search (roam)")
  "w" '(timekeep-visit-node :wk "work file")

  "c" '(nil :wk "clock")
  "r" '(nil :wk "roam/review"))

(+define-leader-keys :prefix "SPC o c" ;; org clocking
  "c" '(org-clock-in-last :wk "clock in (last)")
  "d" (list (general-predicate-dispatch #'org-clock-display
              (not (derived-mode-p 'org-mode))
              (defun +org-clock-display-last (&optional arg)
                "Jump to the latest clock and display clocking info in that buffer."
                (interactive "P")
                (org-clock-goto arg)
                (org-clock-display)))
            :wk "display")
  "i" '(org-clock-in :wk "clock in")
  "o" '(org-clock-out :wk "clock out")
  "r" '(org-resolve-clocks :wk "resolve")
  "g" '(org-clock-goto :wk "goto clock")
  "q" '(org-clock-cancel :wk "cancel"))

(+define-leader-keys :prefix "SPC o r" ;; org roam/review
  "d" '(org-roam-review-list-recently-added :wk "list recent")
  "l" '(org-roam-links :wk "linked nodes")
  "r" '(org-roam-review :wk "review")
  "t" '(org-roam-search-tags :wk "search by tag"))

(+define-leader-keys :prefix "SPC t" ;; toggles
  "b" '(breadcrumb-mode :wk "breadcrumbs (header)")
  "h" '(global-hl-line-mode :wk "highlight line")
  "f" '(global-display-fill-column-indicator-mode :wk "fill column indicator")
  "i" '(indent-bars-mode :wk "indent bars")
  "l" '(global-display-line-numbers-mode :wk "line numbers")
  "m" '(toggle-input-method :wk "input method")
  "s" '(spell-fu-mode :wk "spellchecks")
  "r" '(read-only-mode :wk "readonly")
  "w" '(whitespace-mode :wk "whitespace")
  "v" '(visual-line-mode :wk "line wrapping")
  "V" '(global-visual-line-mode :wk "line wrapping (globally)"))

(+define-leader-keys :prefix "SPC w" ;; windows
  "-" '(+split-window-vertically-dwim :wk "vsplit")
  "/" '(+split-window-horizontally-dwim :wk "hsplit")
  "="  '(balance-windows :wk "balance")
  "d" '(delete-window :wk "delete")
  "o"  '(+delete-nondedicated-windows :wk "delete others")
  "O"  '(delete-other-windows :wk "delete (+dedicated)")
  "q" '(delete-window :wk "delete")
  "r" '(evil-window-rotate-downwards :wk "rotate")
  "s" '(consult-register-load :wk "registers")
  "S" '(window-configuration-to-register :wk "save to reg")
  "t"  '(+toggle-window-dedication :wk "toggle dedication")
  "w" '(other-window :wk "other"))



;; Support multiple SPC-u calls in sequence to chain universal-argument calls.

(keymap-set universal-argument-map "SPC u" #'universal-argument-more)

;; Define alternative M-m key sequence for non-evil modes.

(general-def "M-m" #'+leader-key)

(provide 'mod-leader)

;;; mod-leader.el ends here
