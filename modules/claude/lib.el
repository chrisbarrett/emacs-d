;;; lib.el --- Claude module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for claude-code-ide integration.

;;; Code:

;;;###autoload
(defun +claude-code-ide-active-buffer-p (buf)
  "Return non-nil if BUF is a `claude-code-ide' buffer."
  (and (buffer-live-p buf)
       (string-match-p (rx bol "*claude-code") (buffer-name buf))))

;;;###autoload
(defun +claude-code-ide-scroll-to-bottom-h ()
  "Scroll all visible `claude-code-ide' buffers to bottom.
This ensures consistent positioning when switching tabs, frames, or windows."
  (dolist (window (window-list nil 'no-minibuffer))
    (with-selected-window window
      (when (+claude-code-ide-active-buffer-p (current-buffer))
        (goto-char (point-max))
        (recenter -1)))))

;;;###autoload
(defun +claude-code-eat-remap-nbsp (proc)
  "Remap nobreak-space face in PROC's buffer for `claude-code-ide'.
Prevents non-breaking space characters in prompts from being visible."
  (when-let* ((buf (process-buffer proc)))
    (when (+claude-code-ide-active-buffer-p buf)
      (with-current-buffer buf
        (face-remap-add-relative 'nobreak-space :inherit 'default)))))

;;; lib.el ends here
