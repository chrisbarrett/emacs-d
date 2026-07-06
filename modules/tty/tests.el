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
(load (expand-file-name "lib.el" tty-tests--dir) nil t)

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

;;; Box-drawing display table

;; The box glyphs must live only on per-window tables of TTY frames.  A window
;; table shadows `buffer-display-table' outright (one table per window, no
;; merging), so putting the glyphs on the global `standard-display-table' -- or
;; on the shared per-buffer tables -- leaks `│'/`…' into graphical frames and
;; also hides the `page-break-lines' `?\f' rule.  Keep them window-local and
;; TTY-only.

(ert-deftest tty/box-chars-on-tty-window-table-not-global ()
  "`+tty-frame-use-box-characters' installs box glyphs on a TTY frame's
per-window display table, leaving `standard-display-table' untouched."
  (let ((frame (selected-frame)))
    (skip-unless (not (display-graphic-p frame)))
    (let ((standard-display-table nil))
      (dolist (w (window-list frame 'no-minibuf))
        (set-window-display-table w nil))
      (+tty-frame-use-box-characters frame)
      (let ((dt (window-display-table (frame-selected-window frame))))
        (should dt)
        (should (equal (display-table-slot dt 'vertical-border)
                       (make-glyph-code ?│)))
        (should (equal (display-table-slot dt 'truncation)
                       (make-glyph-code ?… 'warning)))
        ;; The global fallback table never receives box glyphs -> no GUI
        ;; leak.  (`make-display-table' lazily inits the table itself, so we
        ;; assert on the slots rather than on the table being nil.)
        (should (null (and standard-display-table
                           (display-table-slot standard-display-table
                                                'vertical-border))))
        (should (null (and standard-display-table
                           (display-table-slot standard-display-table
                                                'truncation))))))))

(ert-deftest tty/box-chars-skip-graphical-frames ()
  "`+tty-frame-use-box-characters' installs nothing on graphical frames,
so `buffer-display-table' (and thus `page-break-lines') stays visible."
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
    (let ((frame (selected-frame)))
      (dolist (w (window-list frame 'no-minibuf))
        (set-window-display-table w nil))
      (+tty-frame-use-box-characters frame)
      (dolist (w (window-list frame 'no-minibuf))
        (should (null (window-display-table w)))))))

(ert-deftest tty/window-display-table-creates-box-glyphs-and-is-idempotent ()
  "`+tty-window-display-table' creates a box-glyph table once and reuses it."
  (let ((frame (selected-frame)))
    (dolist (w (window-list frame 'no-minibuf))
      (set-window-display-table w nil))
    (let* ((win (frame-selected-window frame))
           (dt (+tty-window-display-table win)))
      (should (equal (display-table-slot dt 'vertical-border)
                     (make-glyph-code ?│)))
      (should (equal (display-table-slot dt 'truncation)
                     (make-glyph-code ?… 'warning)))
      ;; Second call returns the same object rather than replacing it.
      (should (eq dt (+tty-window-display-table win))))))

(provide 'tty-tests)

;;; tty/tests.el ends here
