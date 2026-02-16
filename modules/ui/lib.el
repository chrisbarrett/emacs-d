;;; lib.el --- UI module library functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Autoloaded functions for the ui module.

;;; Code:

(require 'cl-lib)


;;; Tab bar configuration

(defvar +tab-bar-alert-clear-delay 1.0
  "Seconds to wait before clearing alert state on a tab.")

(defvar +tab-bar-alert-pulse-iterations 3
  "Number of times to pulse the alert.")

(defvar +tab-bar-alert-pulse-delay 0.05
  "Delay in seconds between pulse steps.")


;;; Tab bar transient menu

(cl-eval-when (compile)
  (require 'transient))

(with-eval-after-load 'transient
  (transient-define-prefix +tabs-menu--prefix ()
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

;;;###autoload
(defun +tabs-menu ()
  "Transient menu for tab operations."
  (interactive)
  (call-interactively #'+tabs-menu--prefix))


;;; Tab bar alert state management

(defvar +tab-bar--alert-clear-timer nil
  "Timer for clearing alert state on the current tab.")

(defvar +tab-bar--pulse-timers nil
  "List of active pulse timers.")

(defun +tab-bar--cancel-pulse-timers ()
  "Cancel all active pulse timers."
  (mapc #'cancel-timer +tab-bar--pulse-timers)
  (setq +tab-bar--pulse-timers nil))

(defun +tab-bar--blend-colors (color1 color2 alpha)
  "Blend COLOR1 and COLOR2 with ALPHA (0.0 to 1.0).
ALPHA of 0.0 returns COLOR1, 1.0 returns COLOR2."
  (require 'color)
  (let* ((c1 (color-name-to-rgb color1))
         (c2 (color-name-to-rgb color2))
         (r (+ (* (nth 0 c1) (- 1.0 alpha)) (* (nth 0 c2) alpha)))
         (g (+ (* (nth 1 c1) (- 1.0 alpha)) (* (nth 1 c2) alpha)))
         (b (+ (* (nth 2 c1) (- 1.0 alpha)) (* (nth 2 c2) alpha))))
    (color-rgb-to-hex r g b 2)))

(defun +tab-bar--pulse-alert (iterations)
  "Pulse the alert background for ITERATIONS cycles."
  (when (> iterations 0)
    (+tab-bar--cancel-pulse-timers)
    (when (require 'pulsar nil t)
      (let* ((alert-bg (or (face-background 'pulsar-magenta nil t) "#71206a"))
             (default-bg (face-background 'default nil t))
             (steps 10)
             (delay (bound-and-true-p +tab-bar-alert-pulse-delay))
             (total-steps (- (* iterations steps 2) steps)))
        (unless delay (setq delay 0.05))
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
                (let ((blended (+tab-bar--blend-colors default bg a)))
                  (when (facep 'tab-bar-tab-alert)
                    (set-face-attribute 'tab-bar-tab-alert nil :background blended)
                    (set-face-attribute 'tab-bar-tab-inactive-alert nil :background blended)
                    (tab-bar--update-tab-bar-lines))))
              alpha alert-bg default-bg)
             +tab-bar--pulse-timers)))))))

(defun +tab-bar--dispatch-transient-alert (color cycles &optional steps delay gap)
  "Dispatch a transient alert animation on the current tab.
COLOR is the pulse color to use.
CYCLES is the number of complete fade-in/fade-out cycles.
STEPS is the number of animation steps per fade (default 5).
DELAY is seconds between steps (default 0.045).
GAP is seconds between cycles (default 0.1)."
  (+tab-bar--cancel-pulse-timers)
  (when +tab-bar--alert-clear-timer
    (cancel-timer +tab-bar--alert-clear-timer)
    (setq +tab-bar--alert-clear-timer nil))

  (let* ((default-bg (face-background 'tab-bar-tab nil t))
         (steps (or steps 5))
         (delay (or delay 0.045))
         (gap (or gap 0.1))
         (cycle-duration (* delay (* steps 2))))

    (let* ((tabs (frame-parameter nil 'tabs))
           (tab-index (tab-bar--current-tab-index tabs))
           (current-tab (nth tab-index tabs))
           (tab-type (car current-tab))
           (tab-rest (cdr current-tab)))
      (setf (alist-get 'transient-alert tab-rest) t)
      (setf (nth tab-index tabs) (cons tab-type tab-rest))
      (set-frame-parameter nil 'tabs tabs))

    (dotimes (cycle cycles)
      (let ((cycle-offset (+ (* cycle cycle-duration) (* cycle gap))))
        (dotimes (i (* steps 2))
          (let* ((fade-in (< i steps))
                 (step-in-phase (if fade-in i (- (* steps 2) i 1)))
                 (alpha (/ (float step-in-phase) steps))
                 (timer-delay (+ cycle-offset (* delay i))))
            (push
             (run-with-timer
              timer-delay nil
              (lambda (a bg default)
                (let ((blended (+tab-bar--blend-colors default bg a)))
                  (when (facep 'tab-bar-tab)
                    (set-face-attribute 'tab-bar-tab nil :background blended)
                    (tab-bar--update-tab-bar-lines))))
              alpha color default-bg)
             +tab-bar--pulse-timers)))))

    (push
     (run-with-timer
      (+ (* cycles cycle-duration) (* (1- cycles) gap))
      nil
      (lambda ()
        (let* ((tabs (frame-parameter nil 'tabs))
               (tab-index (tab-bar--current-tab-index tabs))
               (current-tab (nth tab-index tabs))
               (tab-type (car current-tab))
               (tab-rest (cdr current-tab)))
          (setf (alist-get 'transient-alert tab-rest) nil)
          (setf (nth tab-index tabs) (cons tab-type tab-rest))
          (set-frame-parameter nil 'tabs tabs))
        (+update-tab-bar-themes)
        (tab-bar--update-tab-bar-lines)))
     +tab-bar--pulse-timers)))

(defun +tab-bar--pulse-tab-switch ()
  "Pulse the current tab briefly when switching."
  (let ((pulse-bg (or (face-background 'pulsar-generic nil t) "#3a3a3a")))
    (+tab-bar--dispatch-transient-alert pulse-bg 1 5 0.035 0)))

;;;###autoload
(defun +tab-bar-set-transient-alert (&optional tab-name color cycles)
  "Set a transient alert on TAB-NAME (or current tab if nil).
COLOR is the pulse color (default pulsar-magenta).
CYCLES is the number of pulses (default 3)."
  (interactive)
  (let* ((tabs (frame-parameter nil 'tabs))
         (tab-index (if tab-name
                        (seq-position tabs tab-name
                                      (lambda (tab name)
                                        (equal (alist-get 'name tab) name)))
                      (tab-bar--current-tab-index tabs)))
         (is-current (eq tab-index (tab-bar--current-tab-index tabs)))
         (color (or color (face-background 'pulsar-magenta nil t) "#71206a"))
         (cycles (or cycles 3)))
    (when tab-index
      (if is-current
          (+tab-bar--dispatch-transient-alert color cycles)
        (let ((current-tab-name (alist-get 'name (tab-bar--current-tab))))
          (tab-bar-select-tab (1+ tab-index))
          (+tab-bar--dispatch-transient-alert color cycles)
          (tab-bar-select-tab-by-name current-tab-name))))))


;;; Tab bar alert set/clear

;;;###autoload
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
      (let* ((tab (nth tab-index tabs))
             (tab-type (car tab))
             (tab-rest (cdr tab))
             (is-current (eq tab-type 'current-tab)))
        (setf (alist-get 'alert tab-rest) t)
        (setf (nth tab-index tabs) (cons tab-type tab-rest))
        (set-frame-parameter nil 'tabs tabs)
        (tab-bar--update-tab-bar-lines)
        (+tab-bar--pulse-alert (or (bound-and-true-p +tab-bar-alert-pulse-iterations) 3))
        (when is-current
          (+tab-bar--schedule-alert-clear))
        t))))

(defun +tab-bar--fade-out-alert (callback)
  "Fade out the alert background, then call CALLBACK."
  (+tab-bar--cancel-pulse-timers)
  (when (require 'pulsar nil t)
    (let* ((alert-bg (or (face-background 'pulsar-magenta nil t) "#71206a"))
           (default-bg (face-background 'default nil t))
           (steps 5)
           (delay 0.05))
      (dotimes (i steps)
        (let* ((progress (/ (float (1+ i)) steps))
               (alpha (- 1.0 progress))
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
      (push
       (run-with-timer
        (* delay steps)
        nil
        callback)
       +tab-bar--pulse-timers))))

;;;###autoload
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
      (+tab-bar--fade-out-alert
       (lambda ()
         (let* ((tabs (frame-parameter nil 'tabs))
                (tab (nth tab-index tabs))
                (tab-type (car tab))
                (tab-rest (cdr tab)))
           (setf (alist-get 'alert tab-rest) nil)
           (setf (nth tab-index tabs) (cons tab-type tab-rest))
           (set-frame-parameter nil 'tabs tabs)
           (+update-tab-bar-themes)
           (tab-bar--update-tab-bar-lines))))
      t)))

(defun +tab-bar--clear-current-alert ()
  "Internal: Clear alert state from the current tab if it has one."
  (let* ((tabs (frame-parameter nil 'tabs))
         (current-tab (assq 'current-tab tabs)))
    (when (and current-tab (alist-get 'alert current-tab))
      (+tab-bar-clear-alert)
      (force-mode-line-update t))))

(defun +tab-bar--cleanup-timers-on-close (tab &optional _deleted)
  "Clean up timers when closing TAB with an alert."
  (when (alist-get 'alert tab)
    (+tab-bar--cancel-pulse-timers)
    (when (eq (car tab) 'current-tab)
      (when +tab-bar--alert-clear-timer
        (cancel-timer +tab-bar--alert-clear-timer)
        (setq +tab-bar--alert-clear-timer nil)))))

(defun +tab-bar--pulse-duration ()
  "Calculate the total duration of the pulse animation in seconds."
  (let ((steps 10)
        (iterations (or (bound-and-true-p +tab-bar-alert-pulse-iterations) 3))
        (delay (or (bound-and-true-p +tab-bar-alert-pulse-delay) 0.05)))
    (* delay (- (* iterations steps 2) steps))))

(defun +tab-bar--schedule-alert-clear ()
  "Internal: Schedule clearing alert on current tab after dwell delay."
  (when +tab-bar--alert-clear-timer
    (cancel-timer +tab-bar--alert-clear-timer)
    (setq +tab-bar--alert-clear-timer nil))
  (let ((total-delay (+ (or (bound-and-true-p +tab-bar-alert-clear-delay) 1.0)
                        (+tab-bar--pulse-duration))))
    (setq +tab-bar--alert-clear-timer
          (run-with-timer total-delay nil
                          #'+tab-bar--clear-current-alert))))


;;; Tab name formatting

(defun +tab-bar-tab-name-format (tab _i)
  "Format TAB name with icon based on worktree type."
  (let* ((is-current (eq (car tab) 'current-tab))
         (has-alert (alist-get 'alert tab))
         (base-face (if is-current
                        'tab-bar-tab
                      'tab-bar-tab-inactive))
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
             (propertize "" 'face (if has-alert
                                      `(:background ,tab-bg :foreground ,tab-fg)
                                    `(:background ,tab-bg :inherit success))))
            ('epic
             (propertize ""
                         'face (if has-alert
                                   `(:background ,tab-bg :foreground ,tab-fg)
                                 `(:background ,tab-bg :inherit font-lock-constant-face))
                         'display '(raise 0.13)))
            ((or 'subagent 'task)
             (propertize "" 'face (if has-alert
                                      `(:background ,tab-bg :foreground ,tab-fg)
                                    `(:background ,tab-bg :inherit font-lock-builtin-face))))
            ('pullreq
             (propertize "" 'face (if has-alert
                                      `(:background ,tab-bg :foreground ,tab-fg)
                                    `(:background ,tab-bg :inherit font-lock-string-face))))
            (_
             (propertize "" 'face (if has-alert
                                      `(:background ,tab-bg :foreground ,tab-fg)
                                    `(:background ,tab-bg :inherit success))))))
         (name (propertize (alist-get 'name tab) 'face face)))
    (apply #'concat `(,pad
                      ,@(when worktree-icon
                          (list worktree-icon pad))
                      ,name))))


;;; Theme-aware tab bar styling

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

;;;###autoload
(defun +update-tab-bar-themes (&rest _)
  "Update tab-bar colors to be distinct and theme-aware."
  (when (facep 'tab-bar)
    (when (require 'pulsar nil t)
      (let* ((default-bg (face-background 'default nil t))
             (dark-theme (if (fboundp '+theme-dark-p) (+theme-dark-p) nil))
             (selected-bg (if dark-theme
                              (face-background 'mode-line nil t)
                            default-bg))
             (tab-bar-bg (if dark-theme
                             (color-lighten-name default-bg +tab-bar-contrast)
                           (color-darken-name default-bg +tab-bar-contrast)))
             (inactive-bg (if dark-theme
                              (color-lighten-name default-bg +inactive-tab-contrast)
                            (color-darken-name default-bg +inactive-tab-contrast)))
             (alert-bg (face-background 'pulsar-magenta nil t))
             (alert-fg (face-foreground 'default nil t))
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
                            :inherit 'shadow)))))


;;; Tab bar hooks

(defun +tab-bar--pulse-new-tab (&optional _tab)
  "Pulse animation when a new tab is opened."
  (let ((pulse-bg (or (face-background 'pulsar-green nil t) "#00c06f")))
    (+tab-bar--dispatch-transient-alert pulse-bg 3 5 0.045 0.1)))

(defun +tab-bar--cleanup-timers-on-delete-frame (_frame)
  "Clean up all alert timers when a frame is deleted."
  (+tab-bar--cancel-pulse-timers)
  (when +tab-bar--alert-clear-timer
    (cancel-timer +tab-bar--alert-clear-timer)
    (setq +tab-bar--alert-clear-timer nil)))


;;; Pulsar macro

;;;###autoload
(defmacro +pulsar--with-eval-pulse (start end &rest body)
  "Pulse a region to indicate whether a command ran or signalled an error.

START and END are the buffer locations to pulse after evaluating BODY.

START & END are evaluated after BODY has completed, and thus after any
buffer modifications have happened."
  (declare (indent 2))
  (let ((gfailed (gensym "failed-"))
        (gerr (gensym "err-"))
        (gstart (gensym "start-"))
        (gend (gensym "end-")))
    `(let (,gfailed)
       (unwind-protect
           (condition-case ,gerr
               (progn ,@body)
             (t
              (setq ,gfailed t)
              (signal (car ,gerr) (cdr ,gerr))))
         (deactivate-mark)
         (when (and (bound-and-true-p pulsar-mode)
                    (require 'pulsar nil t))
           (let ((,gstart ,start)
                 (,gend ,end))
             (when (and ,gstart ,gend)
               (let ((pulse-flag t)
                     (pulse-delay pulsar-delay)
                     (pulse-iterations pulsar-iterations))
                 (pulsar--create-pulse (cons ,gstart ,gend)
                                       (if ,gfailed 'pulsar-red 'pulsar-green))))))))))


;;; Display buffer functions

;;;###autoload
(defun +display-buffer-reuse-non-dedicated-window (buffer alist)
  "Reuse a non-dedicated window for BUFFER when navigating.
Only works for specific navigation commands like dired-find-file,
next-error, etc.  ALIST is display-buffer action alist."
  (when (member this-command '(dired-find-file
                               next-error
                               previous-error
                               compile-goto-error
                               push-button
                               magit-diff-visit-file
                               +magit-diff-visit-file-unselected))
    (let ((candidates (seq-remove (lambda (it)
                                    (or (+side-window-p it)
                                        (window-dedicated-p it)))
                                  (window-list))))
      (pcase candidates
        (`(,sole-window)
         (window--display-buffer buffer sole-window 'reuse alist))))))

;;;###autoload
(defun +display-buffer-fallback (buffer &rest _)
  "Sensible split fallback for displaying BUFFER."
  (when-let* ((win (split-window-sensibly)))
    (with-selected-window win
      (switch-to-buffer buffer)
      (help-window-setup (selected-window)))
    t))


;;; Goto address helper

;;;###autoload
(defun +goto-address-maybe-h ()
  "Enable goto-address-mode unless in org-mode (which handles URLs natively)."
  (unless (derived-mode-p 'org-mode 'org-agenda-mode)
    (goto-address)
    (goto-address-mode +1)))


;;; lib.el ends here
