;; +corelib.el --- Utils copied from Doom -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)


;;; Logging

(defvar +inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `+log' output completely.")

(defvar +log-level
  (if init-file-debug
      (if-let* ((level (getenv-internal "DEBUG"))
                (level (string-to-number level))
                ((not (zerop level))))
          level
        2)
    0)
  "How verbosely to log from `+log' calls.

0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun +log--call (level text &rest args)
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level +log-level)))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat (lambda (x) (format "%s" x)) ":")
           args)))

;; This is a macro instead of a function to prevent the potentially expensive
;; evaluation of its arguments when debug mode is off. Return non-nil.
(defmacro +log (message &rest args)
  "Log a message to stderr or *Messages* (without displaying in the echo area)."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not +inhibit-log)
                (or (not noninteractive)
                    (<= ,level +log-level)))
       (+log--call ,level ,message ,@args))))


;;; setq-hook!

(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun --resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (+unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun --setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'cl-evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "+setq-%s-for-%s-h"
                                          var mode))))))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "+transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (+unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))


(defun +visible-buffers (&optional buffer-list all-frames)
  "Return a list of visible buffers (i.e. not buried)."
  (let ((buffers
	 (delete-dups
	  (cl-loop for frame in (if all-frames (visible-frame-list) (list (selected-frame)))
		   if (window-list frame)
		   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
	(cl-loop for buf in buffers
		 unless (memq buf buffer-list)
		 collect buffers)
      buffers)))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list (delq ,(if fetcher
                          `(funcall ,fetcher ,elt ,list)
                        elt)
                     ,list)))

(provide '+corelib)
