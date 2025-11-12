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



;;; Alert state management

(defvar +tab-bar-alert-clear-delay 1.0
  "Seconds to wait before clearing alert state on a tab.
This delay requires intentional dwelling on a tab before the alert clears,
preventing accidental clearing during rapid navigation.")

(defvar +tab-bar--alert-clear-timer nil
  "Timer for clearing alert state on the current tab.")

(defvar +tab-bar-alert-pulse-iterations 3
  "Number of times to pulse the alert.")

(defvar +tab-bar-alert-pulse-delay 0.05
  "Delay in seconds between pulse steps.")

(defvar +tab-bar--pulse-timers nil
  "List of active pulse timers.")

(defun +tab-bar--cancel-pulse-timers ()
  "Cancel all active pulse timers."
  (mapc #'cancel-timer +tab-bar--pulse-timers)
  (setq +tab-bar--pulse-timers nil))

(defun +tab-bar--blend-colors (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 with ALPHA (0.0 to 1.0).
ALPHA of 0.0 returns COLOR1, 1.0 returns COLOR2."
  (let* ((c1 (color-name-to-rgb color1))
         (c2 (color-name-to-rgb color2))
         (r (+ (* (nth 0 c1) (- 1.0 alpha)) (* (nth 0 c2) alpha)))
         (g (+ (* (nth 1 c1) (- 1.0 alpha)) (* (nth 1 c2) alpha)))
         (b (+ (* (nth 2 c1) (- 1.0 alpha)) (* (nth 2 c2) alpha))))
    (color-rgb-to-hex r g b 2)))

(defun +tab-bar--pulse-alert (iterations)
  "Pulse the alert background for ITERATIONS cycles.
Fades in and out for (iterations - 1) cycles, then ends with a final fade-in
to leave the alert visibly highlighted.
Note: This modifies global alert faces, so only one tab should pulse at a time."
  (when (> iterations 0)
    ;; Cancel any existing pulse timers
    (+tab-bar--cancel-pulse-timers)

    ;; Get the alert colors from the current theme
    (let* ((alert-bg (or (face-background 'pulsar-magenta nil t) "#71206a"))
           (default-bg (face-background 'default nil t))
           (steps 10)  ; Number of steps in fade animation
           (delay +tab-bar-alert-pulse-delay)
           ;; Total steps: full cycles minus the final fade-out
           (total-steps (- (* iterations steps 2) steps)))

      ;; Create fade-in and fade-out sequence, ending on a fade-in
      (dotimes (i total-steps)
        (let* ((cycle-pos (mod i (* steps 2)))
               (fade-in (< cycle-pos steps))
               (step-in-phase (if fade-in cycle-pos (- (* steps 2) cycle-pos 1)))
               (alpha (/ (float step-in-phase) steps))
               (timer-delay (* delay i)))

          (push
           (run-with-timer
            timer-delay nil
            (lambda (a bg default)
              ;; Blend between default and alert background
              (let ((blended (+tab-bar--blend-colors default bg a)))
                ;; Force theme update with blended color
                (when (facep 'tab-bar-tab-alert)
                  (set-face-attribute 'tab-bar-tab-alert nil :background blended)
                  (set-face-attribute 'tab-bar-tab-inactive-alert nil :background blended)
                  (tab-bar--update-tab-bar-lines))))
            alpha alert-bg default-bg)
           +tab-bar--pulse-timers))))))

(defun +tab-bar-set-alert (&optional tab-name)
  "Set alert state on TAB-NAME (or current tab if nil).
This will cause the tab to display with a visually distinct background
until the user dwells on it for `+tab-bar-alert-clear-delay' seconds."
  (interactive)
  (let* ((tabs (frame-parameter nil 'tabs))
         (tab-index (if tab-name
                        (seq-position tabs tab-name
                                      (lambda (tab name)
                                        (equal (alist-get 'name tab) name)))
                      (tab-bar--current-tab-index tabs))))
    (when tab-index
      ;; Get the tab and modify it
      (let* ((tab (nth tab-index tabs))
             ;; Preserve the tab type (current-tab or tab) at the beginning
             (tab-type (car tab))
             (tab-rest (cdr tab))
             (is-current (eq tab-type 'current-tab)))
        ;; Set alert property in the rest of the alist
        (setf (alist-get 'alert tab-rest) t)
        ;; Reconstruct the tab with type first
        (setf (nth tab-index tabs) (cons tab-type tab-rest))
        ;; Update the frame parameter
        (set-frame-parameter nil 'tabs tabs)
        (tab-bar--update-tab-bar-lines)
        ;; Start pulse animation
        (+tab-bar--pulse-alert +tab-bar-alert-pulse-iterations)
        ;; If setting alert on current tab, start the clear timer
        (when is-current
          (+tab-bar--schedule-alert-clear))
        t))))

(defun +tab-bar--fade-out-alert (callback)
  "Fade out the alert background, then call CALLBACK.
Note: This modifies global alert faces."
  ;; Cancel any existing pulse timers
  (+tab-bar--cancel-pulse-timers)

  (let* ((alert-bg (or (face-background 'pulsar-magenta nil t) "#71206a"))
         (default-bg (face-background 'default nil t))
         (steps 5)  ; Quick fade-out
         (delay 0.05))  ; Faster than pulse

    ;; Fade from alert background to default
    (dotimes (i steps)
      (let* ((progress (/ (float (1+ i)) steps))
             (alpha (- 1.0 progress))  ; Fade from 1.0 to 0.0
             (timer-delay (* delay i)))
        (push
         (run-with-timer
          timer-delay nil
          (lambda (a bg default)
            (let ((blended (+tab-bar--blend-colors default bg a)))
              (when (facep 'tab-bar-tab-alert)
                (set-face-attribute 'tab-bar-tab-alert nil :background blended)
                (set-face-attribute 'tab-bar-tab-inactive-alert nil :background blended)
                (tab-bar--update-tab-bar-lines))))
          alpha alert-bg default-bg)
         +tab-bar--pulse-timers)))

    ;; After fade completes, call callback
    (push
     (run-with-timer
      (* delay steps)
      nil
      callback)
     +tab-bar--pulse-timers)))

(defun +tab-bar-clear-alert (&optional tab-name)
  "Clear alert state from TAB-NAME (or current tab if nil).
Fades out the alert before removing it."
  (interactive)
  (let* ((tabs (frame-parameter nil 'tabs))
         (tab-index (if tab-name
                        (seq-position tabs tab-name
                                      (lambda (tab name)
                                        (equal (alist-get 'name tab) name)))
                      (tab-bar--current-tab-index tabs))))
    (when tab-index
      ;; Fade out, then remove the alert property
      (+tab-bar--fade-out-alert
       (lambda ()
         (let* ((tabs (frame-parameter nil 'tabs))
                (tab (nth tab-index tabs))
                (tab-type (car tab))
                (tab-rest (cdr tab)))
           ;; Remove the alert property
           (setf (alist-get 'alert tab-rest) nil)
           ;; Reconstruct the tab with type first
           (setf (nth tab-index tabs) (cons tab-type tab-rest))
           ;; Update the frame parameter
           (set-frame-parameter nil 'tabs tabs)
           ;; Restore original theme colors
           (+update-tab-bar-themes)
           (tab-bar--update-tab-bar-lines))))
      t)))

(defun +tab-bar--clear-current-alert ()
  "Internal: Clear alert state from the current tab if it has one."
  (let* ((tabs (frame-parameter nil 'tabs))
         (current-tab (assq 'current-tab tabs)))
    (when (and current-tab (alist-get 'alert current-tab))
      (+tab-bar-clear-alert)
      ;; Force immediate visual update
      (force-mode-line-update t))))

(defun +tab-bar--cleanup-timers-on-close (tab &optional _deleted)
  "Clean up timers when closing TAB with an alert.
Called by `tab-bar-tab-pre-close-functions'."
  (when (alist-get 'alert tab)
    ;; Cancel pulse timers since they modify global faces
    (+tab-bar--cancel-pulse-timers)
    ;; If closing the current tab, also cancel the alert-clear timer
    (when (eq (car tab) 'current-tab)
      (when +tab-bar--alert-clear-timer
        (cancel-timer +tab-bar--alert-clear-timer)
        (setq +tab-bar--alert-clear-timer nil)))))

(defun +tab-bar--pulse-duration ()
  "Calculate the total duration of the pulse animation in seconds.
Animation does full cycles for (iterations - 1), then a final fade-in."
  (let ((steps 10))  ; Must match +tab-bar--pulse-alert
    (* +tab-bar-alert-pulse-delay
       (- (* +tab-bar-alert-pulse-iterations steps 2)
          steps))))

(defun +tab-bar--schedule-alert-clear ()
  "Internal: Schedule clearing alert on current tab after dwell delay.
Cancels any existing timer first to ensure only one timer runs at a time.
The delay includes the pulse animation duration to avoid interrupting it."
  (when +tab-bar--alert-clear-timer
    (cancel-timer +tab-bar--alert-clear-timer)
    (setq +tab-bar--alert-clear-timer nil))
  (let ((total-delay (+ +tab-bar-alert-clear-delay (+tab-bar--pulse-duration))))
    (setq +tab-bar--alert-clear-timer
          (run-with-timer total-delay nil
                          #'+tab-bar--clear-current-alert))))


;; Left-pad the tab name.

(defun +tab-bar-tab-name-format (tab _i)
  (let* ((is-current (eq (car tab) 'current-tab))
         (has-alert (alist-get 'alert tab))
         (base-face (if is-current
                        'tab-bar-tab
                      'tab-bar-tab-inactive))
         ;; Override face with alert face if alert is set
         (face (if has-alert
                   (if is-current
                       'tab-bar-tab-alert
                     'tab-bar-tab-inactive-alert)
                 base-face))
         (tab-bg (face-attribute face :background))
         (tab-fg (face-attribute face :foreground))

         (pad (propertize " " 'face face))

         (worktree-icon
          (pcase (alist-get 'worktree-type tab)

            (`())
            ('root
             (propertize "" 'face (if has-alert
                                       `(:background ,tab-bg :foreground ,tab-fg)
                                     `(:background ,tab-bg :inherit success))))
            ('epic
             (propertize ""
                         'face (if has-alert
                                   `(:background ,tab-bg :foreground ,tab-fg)
                                 `(:background ,tab-bg :inherit font-lock-constant-face))
                         ;; Manual centring
                         'display '(raise 0.13)))
            ((or 'subagent 'task)
             (propertize "" 'face (if has-alert
                                       `(:background ,tab-bg :foreground ,tab-fg)
                                     `(:background ,tab-bg :inherit font-lock-builtin-face))))
            ('pullreq
             (propertize "" 'face (if has-alert
                                       `(:background ,tab-bg :foreground ,tab-fg)
                                     `(:background ,tab-bg :inherit font-lock-string-face))))
            (_
             (propertize "" 'face (if has-alert
                                       `(:background ,tab-bg :foreground ,tab-fg)
                                     `(:background ,tab-bg :inherit success))))))
         (name (propertize (alist-get 'name tab) 'face face)))
    (apply #'concat `(,pad
                      ,@(when worktree-icon
                          (list worktree-icon pad))
                      ,name))))

(setq tab-bar-tab-name-format-function #'+tab-bar-tab-name-format)


;; Compute tab-bar faces dynamically based on theme.

(defface tab-bar-tab-alert
  '((t :inherit tab-bar-tab))
  "Face for current tab with alert state."
  :group 'tab-bar-faces)

(defface tab-bar-tab-inactive-alert
  '((t :inherit tab-bar-tab-inactive))
  "Face for inactive tab with alert state."
  :group 'tab-bar-faces)

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
                          (color-darken-name default-bg +inactive-tab-contrast)))
           ;; Alert colors - use pulsar faces designed for attention-grabbing
           (alert-bg (or (face-background 'pulsar-magenta nil t)
                         ;; Fallback if pulsar not available
                         (if dark-theme "#71206a" "#FFB6D9")))
           (alert-fg (face-foreground 'default nil t))
           ;; Slightly dimmed for inactive tabs
           (alert-inactive-bg (if dark-theme
                                  (color-darken-name alert-bg 10)
                                (color-lighten-name alert-bg 10))))
      (set-face-attribute 'tab-bar nil
                          :background tab-bar-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,tab-bar-bg))
      (set-face-attribute 'tab-bar-tab nil
                          :background selected-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,selected-bg))
      (set-face-attribute 'tab-bar-tab-inactive nil
                          :background inactive-bg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,inactive-bg)
                          :inherit 'shadow)
      ;; Alert faces
      (set-face-attribute 'tab-bar-tab-alert nil
                          :background alert-bg
                          :foreground alert-fg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,alert-bg)
                          :weight 'bold
                          :inherit 'tab-bar-tab)
      (set-face-attribute 'tab-bar-tab-inactive-alert nil
                          :background alert-inactive-bg
                          :foreground alert-fg
                          :box `(:line-width ,(- +tab-internal-padding) :color ,alert-inactive-bg)
                          :weight 'bold
                          :inherit 'shadow))))


(add-hook '+theme-changed-hook #'+update-tab-bar-themes)
(+update-tab-bar-themes)


;;; Alert clearing hooks

(define-advice tab-bar-select-tab (:after (&rest _) schedule-alert)
  "Schedule alert clearing whenever a tab is selected."
  (+tab-bar--schedule-alert-clear))

;; Pulse newly opened tabs
(defun +tab-bar--pulse-new-tab (&optional _tab)
  "Pulse animation when a new tab is opened.
Called by `tab-bar-tab-post-open-functions'."
  ;; Temporarily set alert property on current tab so it uses alert faces during pulse
  (let* ((tabs (frame-parameter nil 'tabs))
         (tab-index (tab-bar--current-tab-index tabs))
         (current-tab (nth tab-index tabs))
         (tab-type (car current-tab))
         (tab-rest (cdr current-tab)))
    ;; Set alert property temporarily
    (setf (alist-get 'alert tab-rest) t)
    (setf (nth tab-index tabs) (cons tab-type tab-rest))
    (set-frame-parameter nil 'tabs tabs)
    (tab-bar--update-tab-bar-lines)

    ;; Cancel any alert-clear timer since this is a new tab pulse, not an actual alert
    (when +tab-bar--alert-clear-timer
      (cancel-timer +tab-bar--alert-clear-timer)
      (setq +tab-bar--alert-clear-timer nil))

    ;; Do a single pulse cycle (fade in and out)
    (+tab-bar--pulse-alert 1)

    ;; After pulse completes, remove the alert property (no fade-out)
    (run-with-timer
     (+tab-bar--pulse-duration)
     nil
     (lambda ()
       (let* ((tabs (frame-parameter nil 'tabs))
              (tab-index (tab-bar--current-tab-index tabs))
              (current-tab (nth tab-index tabs))
              (tab-type (car current-tab))
              (tab-rest (cdr current-tab)))
         ;; Remove alert property
         (setf (alist-get 'alert tab-rest) nil)
         (setf (nth tab-index tabs) (cons tab-type tab-rest))
         (set-frame-parameter nil 'tabs tabs)
         ;; Restore theme colors
         (+update-tab-bar-themes)
         (tab-bar--update-tab-bar-lines))))))

(add-hook 'tab-bar-tab-post-open-functions #'+tab-bar--pulse-new-tab)

;; Clean up timers when closing tabs with alerts
(add-hook 'tab-bar-tab-pre-close-functions #'+tab-bar--cleanup-timers-on-close)

;; Clean up all timers when frame is deleted
(defun +tab-bar--cleanup-timers-on-delete-frame (_frame)
  "Clean up all alert timers when a frame is deleted.
Called by `delete-frame-functions'."
  (+tab-bar--cancel-pulse-timers)
  (when +tab-bar--alert-clear-timer
    (cancel-timer +tab-bar--alert-clear-timer)
    (setq +tab-bar--alert-clear-timer nil)))

(add-hook 'delete-frame-functions #'+tab-bar--cleanup-timers-on-delete-frame)

(provide 'mod-tabs)

;;; mod-tabs.el ends here
