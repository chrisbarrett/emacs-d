;;; modules/midnight/init.el --- Automatic buffer cleanup at midnight -*- lexical-binding: t; -*-

;;; Commentary:

;; Run a hook at midnight; cleans up old buffers by default. Useful for
;; preventing an Emacs server instance from drowning in open buffers
;; throughout the work week.
;;
;; The behaviour of the buffer cleanup can be customised via the
;; `clean-buffer-list-*' variables.

;;; Code:

(use-package midnight
  :after-call +first-file-hook +first-buffer-hook
  :config
  (midnight-mode +1))

(provide 'modules/midnight/init)

;;; init.el ends here
