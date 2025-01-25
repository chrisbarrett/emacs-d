;; +log.el --- Logging implementation copied from doom -*- lexical-binding: t; -*-

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

(provide '+log)
