;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun +sibling-file-or-other-buffer ()
  (let ((sibling (when-let* ((file (buffer-file-name)))
                   (save-excursion
                     (ignore-errors
                       (find-sibling-file file))))))
    (cond
     ;; Don't show sibling again if it's already visible
     ((get-buffer-window sibling)
      (other-buffer))
     (sibling
      sibling)
     (t
      ;; Show some buffer that's not already visible.
      (or (seq-find (lambda (it)
                      (null (get-buffer-window it)))
                    (buffer-list))
          (other-buffer))))))

(defun +split-window-horizontally-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-horizontally)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (+sibling-file-or-other-buffer))
    (unless arg
      (select-window target-window)))

  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (balance-windows (window-parent)))

(defun +split-window-vertically-dwim (&optional arg)
  "When splitting window, show the other buffer in the new window.

With prefix arg ARG, don't select the new window."
  (interactive "P")
  (split-window-vertically)
  (let ((target-window (next-window)))
    (set-window-buffer target-window (+sibling-file-or-other-buffer))
    (unless arg
      (select-window target-window)))

  (run-hook-with-args 'window-selection-change-functions nil)
  (recenter)
  (balance-windows (window-parent)))

(defun +toggle-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(defun +delete-nondedicated-windows ()
  "Delete windows that are not dedicated."
  (interactive)
  (let ((windows (window-list))
        (selected (selected-window)))
    (dolist (win windows)
      (unless (or (equal win selected)
                  (window-dedicated-p win))
        (delete-window win)))))

(defun +clone-indirect-buffer-of-region (beg end &optional display)
  (interactive (list (region-beginning) (region-end) t))
  (deactivate-mark)
  (let ((old-buf (current-buffer))
        (new-buf))
    (narrow-to-region beg end)
    (setq new-buf (clone-indirect-buffer nil display))
    (with-current-buffer old-buf
      (widen))
    new-buf))


;;; Raise/return side windows

(defvar-local +window-original-side nil)

(defvar +side-window-raised-hook nil)
(defvar +side-window-returned-hook nil)

(defun +side-window-p (window)
  (window-parameter window 'window-side))

(defun +toggle-side-window-raised ()
  "Toggle between side-window and regular window."
  (interactive)
  (cl-labels ((raise-side-window (&optional window)
                (when window
                  (select-window window))
                (let ((buf (current-buffer))
                      (orig-side (window-parameter nil 'window-side)))
                  (delete-window)

                  (switch-to-buffer buf)
                  (setq +window-original-side orig-side)
                  (run-hooks '+side-window-raised-hook)
                  (message "Side window raised")))

              (return-side-window ()
                (let ((buf (current-buffer)))
                  (unless (= 1 (length (seq-remove #'+side-window-p (window-list))))
                    (delete-window))

                  (let ((source-window (selected-window)))
                    (display-buffer-in-side-window buf `((side . ,+window-original-side)))

                    (with-selected-window source-window
                      (previous-buffer))

                    (select-window (get-buffer-window buf))
                    (run-hooks '+side-window-returned-hook)
                    (message "Side window returned")))))

    (cond ((+side-window-p (selected-window))
           (raise-side-window))
          (+window-original-side
           (return-side-window))
          (t
           (pcase (seq-filter #'+side-window-p (window-list))
             (`(,win)
              (raise-side-window win))
             (`()
              (user-error "No side windows to act on"))
             (_
              (user-error "Select a side window")))))))

(defvar +window-return-configuration nil)

(defun +toggle-window-fullframe ()
  "Toggle whether a window takes up the full frame."
  (interactive)
  (cond
   ((+side-window-p (selected-window))
    (setq +window-return-configuration
          (list (current-window-configuration) (point-marker)))

    (let ((buf (current-buffer))
          (orig-side (window-parameter nil 'window-side)))

      ;; Delete this side window, then any remaining side windows.
      (delete-window)
      (let ((ignore-window-parameters t))
        (delete-other-windows))

      (switch-to-buffer buf)
      (setq +window-original-side orig-side)
      (message "Side window raised")))

   ((< 1 (length (window-list)))
    (setq +window-return-configuration
          (list (current-window-configuration) (point-marker)))
    (let ((ignore-window-parameters t))
      (delete-other-windows))
    (message "Focusing window"))

   (+window-return-configuration
    (register-val-jump-to +window-return-configuration nil)
    (setq +window-return-configuration nil)
    (message "Restoring window arrangement"))

   (t
    (user-error "Window already focused"))))

(provide '+window)
