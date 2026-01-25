;;; +consult-imenu-elisp.el --- Split Elisp imenu by visibility -*- lexical-binding: t; -*-

;;; Commentary:

;; Split Emacs Lisp imenu categories by symbol visibility (public vs internal).
;;
;; See specs/0001-consult-imenu-elisp.md for specification.

;;; Code:

(require 'cl-lib)

(defgroup consult-imenu-elisp nil
  "Customization for consult-imenu elisp visibility grouping."
  :group 'consult)

(defcustom +consult-imenu-elisp-excluded-categories
  '("Sections" "Headings" "Package")
  "Categories that pass through unchanged (not split by visibility).
Any category containing \"Section\" or \"Heading\" is also excluded."
  :type '(repeat string)
  :group 'consult-imenu-elisp)

;;; Visibility Classification

(defun +consult-imenu-elisp--internal-p (name)
  "Return non-nil if NAME is an internal symbol.
Internal symbols match `^_' or contain `--'."
  (or (string-prefix-p "_" name)
      (string-match-p "--" name)))

;;; Category Transformation

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

;;; Integration

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

(provide '+consult-imenu-elisp)
;;; +consult-imenu-elisp.el ends here
