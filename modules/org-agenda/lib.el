;;; lib.el --- Utility functions for agenda views  -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for org-agenda configuration.

;;; Code:

(require 'org)

;;; Predicates

;;;###autoload
(defun +agenda--any-scheduled-or-deadline-p ()
  "Return non-nil if entry at point has scheduled time or deadline."
  (or (org-get-scheduled-time (point))
      (org-get-deadline-time (point))))

;;;###autoload
(defun +agenda--skip-heading-safe ()
  "Move to next heading or end of buffer."
  (or (outline-next-heading)
      (goto-char (point-max))))

;;;###autoload
(defun +agenda--scheduled-in-future-p (&optional now)
  "Return non-nil if entry at point is scheduled after NOW."
  (let ((now (or now (current-time))))
    (when-let* ((scheduled (org-get-scheduled-time (point) t)))
      (time-less-p now scheduled))))

;;;###autoload
(defun +agenda--scheduled-now-p (&optional now)
  "Return non-nil if entry at point is scheduled at NOW."
  (let ((now (or now (current-time))))
    (when-let* ((scheduled (org-get-scheduled-time (point) t)))
      (time-equal-p now scheduled))))

;;;###autoload
(defun +agenda--at-TODO-p ()
  "Return non-nil if entry at point has TODO state."
  (equal "TODO" (org-get-todo-state)))

;;;###autoload
(defun +agenda--first-todo-at-this-level-p ()
  "Return non-nil if this is the first TODO among siblings."
  (let (should-skip-entry)
    (unless (+agenda--at-TODO-p)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (+agenda--at-TODO-p)
          (setq should-skip-entry t))))
    should-skip-entry))

;;;###autoload
(defun +agenda--high-priority-p ()
  "Return non-nil if entry at point has priority A."
  (equal ?A (nth 3 (org-heading-components))))

;;;###autoload
(defun +agenda--parent-scheduled-in-future-p ()
  "Return non-nil if any ancestor is scheduled in the future."
  (save-restriction
    (widen)
    (save-excursion
      (let ((found)
            (now (current-time)))
        (while (and (not found) (org-up-heading-safe))
          (setq found (+agenda--scheduled-in-future-p now)))
        found))))

;;; Skip Functions

;;;###autoload
(defun +agenda-view-skip-function ()
  "Skip archived entries in agenda view."
  (let ((tags (org-get-tags)))
    (when (seq-contains-p tags "ARCHIVE")
      (+agenda--skip-heading-safe))))

;;;###autoload
(defun +agenda-next-actions-skip-function ()
  "Skip function for Next Actions view.
Scheduled items will show in the agenda view and should be skipped here.
Always include high-priority TODOs.
Only show first TODO at each level."
  (cond
   ;; Scheduled items will show in the agenda view and should be skipped here.
   ((or (+agenda--any-scheduled-or-deadline-p)
        (+agenda--parent-scheduled-in-future-p))
    (+agenda--skip-heading-safe))

   ;; Always include high-priority todos
   ((and (+agenda--high-priority-p) (+agenda--at-TODO-p))
    nil)

   ((+agenda--first-todo-at-this-level-p)
    (+agenda--skip-heading-safe))))

;;; Agenda Files Update

(defvar +org--agenda-update-process nil
  "Process for updating agenda files.")

(defconst +agenda-files-update-script
  (file-name-concat user-emacs-directory "scripts/update-agenda-files.sh")
  "Path to script that updates org-agenda-files.")

;;;###autoload
(defun +org-agenda-update-files ()
  "Update org-agenda-files by scanning org-directory."
  (unless (and +org--agenda-update-process (process-live-p +org--agenda-update-process))
    (setq +org--agenda-update-process
          (start-process "update-org-agenda-files" nil +agenda-files-update-script))))

;;; Habit Graph Resizing

(defvar +org-habit-graph-window-ratio 0.2
  "The ratio of the consistency graphs relative to the window width.")

(defvar +org-habit-graph-padding 2
  "The padding added to the end of the consistency graph.")

(defvar +org-habit-min-width 30
  "Hide the consistency graph if `org-habit-graph-column' is less than this.")

;;;###autoload
(defun +org-habit-resize-graph-h ()
  "Right align and resize the consistency graphs.
Based on `+org-habit-graph-window-ratio'."
  (require 'org-habit)
  (let* ((total-days (float (+ org-habit-preceding-days org-habit-following-days)))
         (preceding-days-ratio (/ org-habit-preceding-days total-days))
         (graph-width (floor (* (window-width) +org-habit-graph-window-ratio)))
         (preceding-days (floor (* graph-width preceding-days-ratio)))
         (following-days (- graph-width preceding-days))
         (graph-column (- (window-width) (+ preceding-days following-days)))
         (graph-column-adjusted (if (> graph-column +org-habit-min-width)
                                    (- graph-column +org-habit-graph-padding)
                                  nil)))
    (setq-local org-habit-preceding-days preceding-days)
    (setq-local org-habit-following-days following-days)
    (setq-local org-habit-graph-column graph-column-adjusted)))



;;; lib.el ends here
