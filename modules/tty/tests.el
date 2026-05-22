;;; tty/tests.el --- Tests for tty module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for terminal-frame compatibility, focused on the colour-theming
;; sentinel contract documented in `openspec/specs/tty/spec.md'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load `early-init.el' to pick up `+sync-frame-parameters', the producer
;; whose invariants we're asserting against.
(defvar tty-tests--dir (file-name-directory (or load-file-name buffer-file-name)))
(load (expand-file-name "../../early-init.el" tty-tests--dir) nil t)

;; Forward-declare the cache var so `let' creates a dynamic binding visible to
;; `boundp' inside `+sync-frame-parameters'.  The real defvar lives in
;; `modules/tty/init.el', which we don't load here.
(defvar +theme-default-background nil
  "Test-side forward declaration; see `modules/tty/init.el'.")

(defun tty-tests--faces-with-attr-value (attr value)
  "Return faces whose ATTR equals VALUE (string compare)."
  (cl-loop for face in (face-list)
           when (equal (face-attribute face attr nil) value)
           collect face))

(defmacro tty-tests--with-fringe-faces-saved (&rest body)
  "Run BODY with fringe and window-divider face state restored after."
  (declare (indent 0))
  `(let* ((faces '(fringe
                   window-divider
                   window-divider-first-pixel
                   window-divider-last-pixel))
          (saved (mapcar (lambda (f)
                           (cons f (mapcar (lambda (a)
                                             (cons a (face-attribute f a nil)))
                                           '(:foreground :background))))
                         faces)))
     (unwind-protect
         (progn ,@body)
       (dolist (entry saved)
         (let ((face (car entry)))
           (face-spec-reset-face face)
           (dolist (attr-cell (cdr entry))
             (let ((attr (car attr-cell))
                   (val (cdr attr-cell)))
               (unless (eq val 'unspecified)
                 (set-face-attribute face nil attr val)))))))))

(ert-deftest tty/sentinel-not-stamped-on-foregrounds-when-cache-unspecified ()
  "`+sync-frame-parameters' skips fringe/divider paint when only the
sentinel is available, leaving no face with `:foreground
\"unspecified-bg\"' (the cross-stamp hazard)."
  (tty-tests--with-fringe-faces-saved
    (let ((+theme-default-background nil))
      (cl-letf (((symbol-function 'face-attribute)
                 (lambda (face attr &optional _frame &rest _)
                   (cond ((and (eq face 'default) (eq attr :background))
                          "unspecified-bg")
                         (t 'unspecified)))))
        (+sync-frame-parameters nil)))
    (should (null (tty-tests--faces-with-attr-value :foreground "unspecified-bg")))
    (should (null (tty-tests--faces-with-attr-value :background "unspecified-fg")))))

(ert-deftest tty/cache-bg-stamped-when-real-colour-available ()
  "`+sync-frame-parameters' paints fringe/divider :foreground from the
real cached theme bg, never the sentinel."
  (tty-tests--with-fringe-faces-saved
    (let ((+theme-default-background "#fdf6e3"))
      (cl-letf (((symbol-function 'face-attribute)
                 (lambda (face attr &optional _frame &rest _)
                   (cond ((and (eq face 'default) (eq attr :background))
                          "unspecified-bg")
                         (t 'unspecified)))))
        (+sync-frame-parameters nil)))
    (dolist (face '(fringe
                    window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (should (equal (face-attribute face :foreground nil) "#fdf6e3")))))

(ert-deftest tty/no-cross-stamped-sentinels-after-sync ()
  "Cross-cutting invariant: regardless of the default-bg value passed
through `+sync-frame-parameters', the bg-sentinel never leaks onto
any face's `:foreground' and the fg-sentinel never leaks onto any
face's `:background' anywhere in `(face-list)'."
  (tty-tests--with-fringe-faces-saved
    (dolist (cache-val '(nil "unspecified-bg" "#fdf6e3" "#000000"))
      (let ((+theme-default-background cache-val))
        (cl-letf (((symbol-function 'face-attribute)
                   (lambda (face attr &optional _frame &rest _)
                     (cond ((and (eq face 'default) (eq attr :background))
                            "unspecified-bg")
                           (t 'unspecified)))))
          (+sync-frame-parameters nil)))
      (should (null (tty-tests--faces-with-attr-value :foreground "unspecified-bg")))
      (should (null (tty-tests--faces-with-attr-value :background "unspecified-fg"))))))

(provide 'tty-tests)

;;; tty/tests.el ends here
