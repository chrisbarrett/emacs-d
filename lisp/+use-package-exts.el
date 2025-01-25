;;; +use-package-exts --- Extra keywords for use-package -*- lexical-binding: t; -*-

;;; Commentary:

;; Extend use-package to support incremental loading of deferred packages. This
;; makes it more likely that Emacs will have already loaded a feature in the
;; background by the time you attempt to use it in your session.
;;
;; The two added keywords are `:defer-incrementally' and `:after-call'.
;;
;; This functionality is taken from Doom; see the use-package module.

;;; Code:

(eval-when-compile
  (require 'use-package-core))

(defvar +use-package--deferred-alist '(t))

(with-eval-after-load 'use-package-core

  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  ;;; :defer-incrementally

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)

  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((doom-load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  ;;; :after-call

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)

  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (condition-case e
                       ;; If `default-directory' is a directory that doesn't
                       ;; exist or is unreadable, Emacs throws up file-missing
                       ;; errors, so we set it to a directory we know exists and
                       ;; is readable.
                       (let ((default-directory user-emacs-directory))
                         (require ',name))
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name +use-package--deferred-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (delq! deferral-list +use-package--deferred-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name +use-package--deferred-alist)
             (push '(,name) +use-package--deferred-alist))
           (nconc (assq ',name +use-package--deferred-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state)))))
  )

(provide '+use-package-exts)
