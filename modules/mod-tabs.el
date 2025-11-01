;;; mod-tabs.el --- Tab management with transient menu -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides a transient menu for tab management operations
;; and theme-aware tab bar styling.

;;; Code:

(require '+theme)
(require 'cl-lib)
(require 'color)
(require 'tab-bar)

(cl-eval-when (compile)
  (require 'transient))

(with-eval-after-load 'transient
  (transient-define-prefix +tabs-menu ()
    "Transient menu for tab operations."
    [["Navigation"
      ("j" "Next tab" tab-bar-switch-to-next-tab)
      ("l" "Next tab" tab-bar-switch-to-next-tab :if (lambda () nil))
      ("k" "Previous tab" tab-bar-switch-to-prev-tab)
      ("h" "Previous tab" tab-bar-switch-to-prev-tab :if (lambda () nil))
      ("s" "Select tab by name" tab-bar-switch-to-tab)]
     ["Management"
      ("c" "Create new tab" tab-bar-new-tab)
      ("d" "Close tab" tab-bar-close-tab)
      ("x" "Close tab" tab-bar-close-tab :if (lambda () nil))
      ("r" "Rename tab" tab-bar-rename-tab)
      ("m" "Move tab" tab-bar-move-tab)]
     ["Other"
      ("u" "Undo close tab" tab-bar-undo-close-tab)
      ("o" "Close other tabs" tab-bar-close-other-tabs)]]))


;; Left-pad the tab name.

(defun +tab-bar-tab-name-format (tab _i)
  (let* ((face (if (eq (car tab) 'current-tab)
                   'tab-bar-tab
                 'tab-bar-tab-inactive))
         (tab-bg (face-attribute face :background))

         (pad (propertize " " 'face face))

         (worktree-icon
          (pcase (alist-get 'worktree-type tab)

            (`())
            ('root
             (propertize "" 'face `(:background ,tab-bg :inherit success)))
            ('epic
             (propertize "" 'face `(:background ,tab-bg :inherit font-lock-constant-face)))
            ('subagent
             (propertize "" 'face `(:background ,tab-bg :inherit font-lock-builtin-face)))
            (_
             (propertize "" 'face `(:background ,tab-bg :inherit success)))))
         (name (propertize (alist-get 'name tab) 'face face)))
    (apply #'concat `(,pad
                      ,@(when worktree-icon
                          (list worktree-icon pad))
                      ,name))))

(setq tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)


;; Compute tab-bar faces dynamically based on theme.

(defconst +tab-bar-contrast 5)
(defconst +inactive-tab-contrast 5)
(defconst +tab-internal-padding 1)

(defun +update-tab-bar-themes (&rest _)
  "Update tab-bar colors to be distinct and theme-aware."
  (when (facep 'tab-bar)
    (let* ((default-bg (face-background 'default nil t))
           ;; Detect if theme is dark by checking if background is dark
           (dark-theme (+theme-dark-p))
           ;; In dark mode, use mode-line bg for selected tab; in light mode use default
           (selected-bg (if dark-theme
                            (face-background 'mode-line nil t)
                          default-bg))
           (tab-bar-bg (if dark-theme
                           (color-lighten-name default-bg +tab-bar-contrast)
                         (color-darken-name default-bg +tab-bar-contrast)))
           (inactive-bg (if dark-theme
                            (color-lighten-name default-bg +inactive-tab-contrast)
                          (color-darken-name default-bg +inactive-tab-contrast))))
      (set-face-attribute 'tab-bar nil
                          :background tab-bar-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,tab-bar-bg))
      (set-face-attribute 'tab-bar-tab nil
                          :background selected-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,selected-bg))
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background inactive-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,inactive-bg)
                          :inherit 'shadow))))


(add-hook '+theme-changed-hook #'+update-tab-bar-themes)
(+update-tab-bar-themes)

(provide 'mod-tabs)

;;; mod-tabs.el ends here
