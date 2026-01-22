;;; init-elpaca.el --- Boot & configure elpaca package manager -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)
(require 'use-package-core)

(defvar elpaca-repos-directory nil)

(defconst +chrisbarrett-elpaca-repos
  (seq-map (lambda (repo)
             (file-name-concat elpaca-repos-directory (concat repo "/")))
           '("emacs-beads" "nursery")))

(dolist (repo +chrisbarrett-elpaca-repos)
  (add-to-list 'trusted-content (abbreviate-file-name repo)))

;; Configure aspects of elpaca not required for initial package bootstrap.

(use-package elpaca
  :general-config
  (:states 'normal :keymaps 'elpaca-manager-mode-map "/" 'elpaca-ui-search)
  (:keymaps 'elpaca-info-mode-map "q" 'quit-window)

  :config
  (+dirlocals-set (list elpaca-repos-directory
                        elpaca-builds-directory)
    '((nil . ((mode . read-only))))))


;;; :ensure-unless-local use-package keyword

;; Provides a use-package keyword that checks for a local checkout before
;; installing via Elpaca. Usage:
;;
;;   (use-package foo
;;     :ensure-unless-local ("~/src/foo" (foo :host github :repo "user/foo"))
;;     ...)
;;
;; If the local path exists, it's added to load-path. Otherwise, Elpaca
;; installs the package.

(defun use-package-normalize/:ensure-unless-local (_name _keyword args)
  (let ((arg (car args)))
    (unless (and (listp arg)
                 (stringp (car arg))
                 (listp (cadr arg)))
      (use-package-error
       ":ensure-unless-local expects (LOCAL-PATH ELPACA-RECIPE)"))
    arg))

(defun use-package-handler/:ensure-unless-local (name _keyword arg rest state)
  (let ((local-path (car arg))
        (recipe (cadr arg)))
    (use-package-concat
     `((let ((local-path (expand-file-name ,local-path)))
         (if (file-directory-p local-path)
             (add-to-list 'load-path local-path)
           (elpaca ,recipe))))
     (use-package-process-keywords name rest state))))

;; Insert keyword before :ensure
(unless (memq :ensure-unless-local use-package-keywords)
  (let ((tail (memq :ensure use-package-keywords)))
    (setcdr tail (cons :ensure-unless-local (cdr tail)))))

(provide 'init-elpaca)

;;; init-elpaca.el ends here
