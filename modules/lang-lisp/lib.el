;;; lib.el --- Emacs Lisp development library -*- lexical-binding: t; -*-

;;; Commentary:

;; Library functions for Emacs Lisp development.

;;; Code:

(require 'cl-lib)

;;; consult-imenu Visibility Grouping

(defgroup consult-imenu-elisp nil
  "Customization for consult-imenu elisp visibility grouping."
  :group 'consult)

(defcustom +consult-imenu-elisp-excluded-categories
  '("Sections" "Headings" "Package")
  "Categories that pass through unchanged (not split by visibility).
Any category containing \"Section\" or \"Heading\" is also excluded."
  :type '(repeat string)
  :group 'consult-imenu-elisp)

(defun +consult-imenu-elisp--internal-p (name)
  "Return non-nil if NAME is an internal symbol.
Internal symbols match `^_' or contain `--'."
  (or (string-prefix-p "_" name)
      (string-match-p "--" name)))

(defun +consult-imenu-elisp--excluded-category-p (category)
  "Return non-nil if CATEGORY should not be split by visibility."
  (or (member category +consult-imenu-elisp-excluded-categories)
      (string-match-p "Section\\|Heading" category)))

(defun +consult-imenu-elisp--split-items (items)
  "Split ITEMS into (PUBLIC . INTERNAL) lists based on symbol visibility.
ITEMS is a list of (NAME . POSITION) pairs."
  (let (public internal)
    (dolist (item items)
      (if (+consult-imenu-elisp--internal-p (car item))
          (push item internal)
        (push item public)))
    (cons (nreverse public) (nreverse internal))))

(defun +consult-imenu-elisp--transform-category (category items)
  "Transform CATEGORY with ITEMS into visibility-grouped categories.
Returns a list of (CATEGORY-NAME . ITEMS) entries.
Empty groups are omitted."
  (if (+consult-imenu-elisp--excluded-category-p category)
      ;; Pass through unchanged
      (list (cons category items))
    ;; Split by visibility
    (let* ((split (+consult-imenu-elisp--split-items items))
           (public (car split))
           (internal (cdr split))
           (result '()))
      ;; Public first, then internal (order matters per spec)
      (when public
        (push (cons (concat category " (Public)") public) result))
      (when internal
        (push (cons (concat category " (Internal)") internal) result))
      (nreverse result))))

(defun +consult-imenu-elisp--transform-alist (alist)
  "Transform imenu ALIST by splitting categories by visibility.
ALIST is in the format returned by `imenu--make-index-alist':
  ((CATEGORY . ((NAME . POSITION) ...)) ...)
  or (NAME . POSITION) for top-level items."
  (let ((result '()))
    (dolist (entry alist)
      (if (and (consp entry)
               (consp (cdr entry))
               (listp (cdr entry))
               ;; Check if it's a category (cdr is a list of items, not a position)
               (consp (cadr entry)))
          ;; Category entry: (CATEGORY . ((NAME . POS) ...))
          (let ((category (car entry))
                (items (cdr entry)))
            (setq result (nconc result (+consult-imenu-elisp--transform-category category items))))
        ;; Top-level item: (NAME . POS) - pass through unchanged
        (push entry result)))
    ;; Preserve order for non-category items
    (nreverse result)))

(defun +consult-imenu-elisp--advice (orig-fn)
  "Advice for `consult-imenu--compute' to transform elisp imenu.
ORIG-FN is the original function."
  (let ((items (funcall orig-fn)))
    (if (derived-mode-p 'emacs-lisp-mode)
        (+consult-imenu-elisp--transform-alist items)
      items)))

;;;###autoload
(defun +consult-imenu-elisp-enable ()
  "Enable visibility grouping for Emacs Lisp in consult-imenu."
  (interactive)
  (advice-add 'consult-imenu--compute :around #'+consult-imenu-elisp--advice))

;;;###autoload
(defun +consult-imenu-elisp-disable ()
  "Disable visibility grouping for Emacs Lisp in consult-imenu."
  (interactive)
  (advice-remove 'consult-imenu--compute #'+consult-imenu-elisp--advice))

;;; Eval Commands

;;;###autoload
(defun +elisp-eval-dwim (&optional beg end)
  "Perform a context-sensitive Elisp eval action.

Evaluate either the current region, from BEG to END, or the defun (or
top-level-form) at point."
  (interactive (when (region-active-p)
                 (list (region-beginning) (region-end))))
  (if (and beg end)
      (message "Eval region => %s" (eval-region beg end))
    (message "Eval defun => %s" (eval-defun nil))))

;;;###autoload
(defun +elisp-eval-buffer ()
  "Evaluate the current buffer with additional visual feedback."
  (interactive)
  (let ((inhibit-redisplay t))
    (call-interactively #'eval-buffer)
    (message "Buffer evaluated")))

;;; ERT Runner

;;;###autoload
(defun +ert (arg)
  "Run ERT tests.
With prefix ARG, run all tests; otherwise prompt for selector."
  (interactive "p")
  (if arg
      (ert t)
    (call-interactively #'ert)))

;;; Evil Lookup

(defun +emacs-lisp-lookup-func ()
  "Lookup symbol at point using helpful or describe-symbol."
  (if (require 'helpful nil t)
      (helpful-at-point)
    (describe-symbol (symbol-at-point))))

(provide 'lang-lisp-lib)
;;; lib.el ends here
