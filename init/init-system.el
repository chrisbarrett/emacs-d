;;; init-system.el --- Host system integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require '+corelib)

;; Run a hook at midnight; cleans up old buffers by default. Useful for
;; preventing an Emacs server instance from drowning in open buffers
;; throughout the work week.
;;
;; The behaviour of the buffer cleanup can be customised via the
;; `clean-buffer-list-*' variables.
(use-package midnight
  :after-call +first-file-hook +first-buffer-hook
  :config
  (midnight-mode +1))


(provide 'init-system)

;;; init-system.el ends here
