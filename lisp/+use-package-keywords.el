;;; +use-package-keywords.el --- Custom use-package keywords -*- lexical-binding: t; -*-

;;; Commentary:

;; Custom use-package keywords for this configuration.
;;
;; Keywords:
;;
;; :after-call SYMBOL|LIST
;;   Takes a symbol or list of symbols representing functions or hook variables.
;;   The first time any of these functions or hooks are executed, the package is
;;   loaded.
;;
;; :defer-incrementally SYMBOL|LIST|t
;;   Takes a symbol or list of symbols representing packages that will be loaded
;;   incrementally at startup before this one. This is helpful for large packages
;;   like magit or org, which load a lot of dependencies on first load. This lets
;;   you load them piece-meal during idle periods, so that when you finally do need
;;   the package, it'll load quicker.
;;
;;   NAME is implicitly added if this property is present and non-nil. No need to
;;   specify it. A value of `t' implies NAME.
;;
;; :ensure-unless-local (LOCAL-PATH ELPACA-RECIPE)
;;   Checks for a local checkout before installing via Elpaca. If the local path
;;   exists, it's added to load-path. Otherwise, Elpaca installs the package.
;;
;;   Example:
;;     (use-package foo
;;       :ensure-unless-local ("~/src/foo" (foo :host github :repo "user/foo"))
;;       ...)

;;; Code:

(require '+corelib)

(eval-when-compile
  (require 'use-package-core))

(declare-function use-package-process-keywords "use-package-core")
(declare-function use-package-normalize-symlist "use-package-core")
(declare-function use-package-list-insert "use-package-core")



;;; Incremental loading

(defvar +load-packages--work-queue '(t))

(defvar +load-packages--first-idle-timer (if (daemonp) 0 2.0)
  "How long (in idle seconds) until incremental loading starts.

Set this to nil to disable incremental loading at startup. Set this to 0
to load all incrementally deferred packages immediately at
`after-init-hook'.")

(defvar +load-packages--incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defun +load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, PACKAGES will be marked for incremental loading next
time Emacs is idle for `+load-packages--first-idle-timer' seconds (falls
back to `+load-packages--incremental-idle-timer'), then in
`+load-packages--incremental-idle-timer' intervals afterwards."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (first-idle-timer (or +load-packages--first-idle-timer
                               +load-packages--incremental-idle-timer))
         (packages (flatten-tree packages)))
    (if (not now)
        (cl-callf append +load-packages--work-queue packages)
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              (+log "start:iloader: Already loaded %s (%d left)" req (length packages))
            (condition-case-unless-debug e
                (and
                 (or (null (setq idle-time (current-idle-time)))
                     (< (float-time idle-time) first-idle-timer)
                     (not
                      (while-no-input
                        (+log "start:iloader: Loading %s (%d left)" req (length packages))
                        ;; If `default-directory' doesn't exist or is
                        ;; unreadable, Emacs throws file errors.
                        (let ((default-directory user-emacs-directory)
                              (inhibit-message t)
                              (file-name-handler-alist
                               (list (rassq 'jka-compr-handler file-name-handler-alist))))
                          (require req nil t)
                          t))))
                 (push req packages))
              (error
               (message "Error: failed to incrementally load %S because: %s" req e)
               (setq packages nil)))
            (if (null packages)
                (+log "start:iloader: Finished!")
              (run-at-time (if idle-time
                               +load-packages--incremental-idle-timer
                             first-idle-timer)
                           nil #'+load-packages-incrementally
                           packages t)
              (setq packages nil))))))))

(defun +load-packages-incrementally-h ()
  "Begin incrementally loading packages in `+load-packages--work-queue'.

If this is a daemon session, load them all immediately instead."
  (when (numberp +load-packages--first-idle-timer)
    (if (zerop +load-packages--first-idle-timer)
        (mapc #'require (cdr +load-packages--work-queue))
      (run-with-idle-timer +load-packages--first-idle-timer
                           nil #'+load-packages-incrementally
                           (cdr +load-packages--work-queue) t))))



;;; Keyword setup

(defun +use-package-keywords-setup ()
  "Set up custom use-package keywords."
  (require 'use-package-core)

  ;; Register incremental loading keywords
  (dolist (keyword '(:defer-incrementally :after-call))
    (push keyword use-package-deferring-keywords)
    (setq use-package-keywords
          (use-package-list-insert keyword use-package-keywords :after)))

  ;; Register :ensure-unless-local before :ensure
  (unless (memq :ensure-unless-local use-package-keywords)
    (let ((tail (memq :ensure use-package-keywords)))
      (setcdr tail (cons :ensure-unless-local (cdr tail)))))

  ;;; :defer-incrementally

  (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)

  (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
    (use-package-concat
     `((+load-packages-incrementally
        ',(if (equal targets '(t))
              (list name)
            (append targets (list name)))))
     (use-package-process-keywords name rest state)))

  ;;; :after-call

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)

  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "+after-call-%s-h" name))))
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
              (when-let* ((deferral-list (assq ',name +load-packages--work-queue)))
                (dolist (hook (cdr deferral-list))
                  (advice-remove hook #',fn)
                  (remove-hook hook #',fn))
                (delq! deferral-list +load-packages--work-queue)
                (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name +load-packages--work-queue)
             (push '(,name) +load-packages--work-queue))
           (nconc (assq ',name +load-packages--work-queue)
                  '(,@hooks)))
         (use-package-process-keywords name rest state)))))

  ;;; :ensure-unless-local

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
       (use-package-process-keywords name rest state)))))

(provide '+use-package-keywords)

;;; +use-package-keywords.el ends here
