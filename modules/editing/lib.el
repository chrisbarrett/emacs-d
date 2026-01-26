;;; lib.el --- Editing module autoloaded functions.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for the editing module.
;; Provides auto-revert functionality that bypasses file watchers.

;;; Code:

(require '+corelib)

;;;###autoload
(defun +auto-revert-current-buffer-h ()
  "Revert current buffer if it is file-backed and stale.
Skips non-file buffers, internal buffers, and buffers already in
`auto-revert-mode'.  Used in place of file-watcher based auto-revert."
  (unless (or auto-revert-mode
              (active-minibuffer-window)
              (not (buffer-file-name))
              (string-prefix-p " " (buffer-name)))
    (let ((auto-revert-mode t))
      (auto-revert-handler))))

;;;###autoload
(defun +auto-revert-visible-buffers-h ()
  "Auto revert stale buffers in visible windows, if necessary."
  (dolist (buf (+visible-buffers))
    (with-current-buffer buf
      (+auto-revert-current-buffer-h))))

(provide 'editing-lib)

;;; lib.el ends here
