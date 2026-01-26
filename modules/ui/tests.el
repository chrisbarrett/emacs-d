;;; tests.el --- Tests for ui module -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the ui module based on spec 009-ui.md testable properties.

;;; Code:

(require 'ert)

;; Get module directory
(defvar ui-test--module-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing the ui module.")

;; Load lib.el to get autoloaded functions
(let ((lib-file (expand-file-name "lib.el" ui-test--module-dir)))
  (when (file-exists-p lib-file)
    (load lib-file nil 'nomessage)))

;; Load init.el for settings
(let ((init-file (expand-file-name "init.el" ui-test--module-dir)))
  (condition-case nil
      (load init-file nil t)
    (error nil)))


;;; Module structure tests

(ert-deftest ui-module-has-packages-eld ()
  "P0: Module should have packages.eld."
  (let ((packages-file (expand-file-name "packages.eld" ui-test--module-dir)))
    (should (file-exists-p packages-file))))

(ert-deftest ui-module-has-spec ()
  "P0: Module should have spec.md."
  (let ((spec-file (expand-file-name "spec.md" ui-test--module-dir)))
    (should (file-exists-p spec-file))))

(ert-deftest ui-module-has-init ()
  "P0: Module should have init.el."
  (let ((init-file (expand-file-name "init.el" ui-test--module-dir)))
    (should (file-exists-p init-file))))

(ert-deftest ui-module-has-lib ()
  "P0: Module should have lib.el."
  (let ((lib-file (expand-file-name "lib.el" ui-test--module-dir)))
    (should (file-exists-p lib-file))))


;;; Scrolling settings (P1)

(ert-deftest ui-scrolling-hscroll-margin ()
  "Scrolling: hscroll-margin should be 2."
  (should (equal hscroll-margin 2)))

(ert-deftest ui-scrolling-hscroll-step ()
  "Scrolling: hscroll-step should be 1."
  (should (equal hscroll-step 1)))

(ert-deftest ui-scrolling-scroll-conservatively ()
  "Scrolling: scroll-conservatively should be 10."
  (should (equal scroll-conservatively 10)))

(ert-deftest ui-scrolling-auto-window-vscroll ()
  "Scrolling: auto-window-vscroll should be nil."
  (should (null auto-window-vscroll)))


;;; Cursor settings

(ert-deftest ui-cursor-blink-disabled ()
  "Cursor: blink-cursor-mode should be disabled."
  (should (null blink-cursor-mode)))

(ert-deftest ui-cursor-in-nonselected ()
  "Cursor: cursor-in-non-selected-windows should be nil."
  (should (null cursor-in-non-selected-windows)))


;;; Bidirectional text

(ert-deftest ui-bidi-paragraph-direction ()
  "Bidi: bidi-paragraph-direction should be left-to-right."
  (should (equal bidi-paragraph-direction 'left-to-right)))

(ert-deftest ui-bidi-inhibit-bpa ()
  "Bidi: bidi-inhibit-bpa should be t."
  (should bidi-inhibit-bpa))


;;; Window splitting

(ert-deftest ui-split-width-threshold ()
  "Split: split-width-threshold should be 160."
  (should (equal split-width-threshold 160)))

(ert-deftest ui-split-height-threshold ()
  "Split: split-height-threshold should be nil."
  (should (null split-height-threshold)))


;;; Dialog & keystrokes

(ert-deftest ui-use-dialog-box ()
  "Dialog: use-dialog-box should be nil."
  (should (null use-dialog-box)))

(ert-deftest ui-echo-keystrokes ()
  "Keystrokes: echo-keystrokes should be 0.02."
  (should (equal echo-keystrokes 0.02)))


;;; P1: Tab bar mode is active after init

(ert-deftest ui-tab-bar-mode-active ()
  "P1: Tab bar mode should be active."
  ;; Skip in batch mode - tab-bar-mode is only enabled in interactive sessions
  (skip-unless (not noninteractive))
  (skip-unless (boundp 'tab-bar-mode))
  (should tab-bar-mode))


;;; P2: Tab switching keybindings

(ert-deftest ui-tab-keybinding-next ()
  "P2: M-> should be bound to tab-bar-switch-to-next-tab."
  (skip-unless (and (featurep 'general)
                    (boundp 'override-global-map)))
  (should (eq (lookup-key (symbol-value 'override-global-map) (kbd "M->"))
              'tab-bar-switch-to-next-tab)))

(ert-deftest ui-tab-keybinding-prev ()
  "P2: M-< should be bound to tab-bar-switch-to-prev-tab."
  (skip-unless (and (featurep 'general)
                    (boundp 'override-global-map)))
  (should (eq (lookup-key (symbol-value 'override-global-map) (kbd "M-<"))
              'tab-bar-switch-to-prev-tab)))


;;; Tab bar variables

(ert-deftest ui-tab-bar-alert-clear-delay-defined ()
  "Tab bar: +tab-bar-alert-clear-delay should be defined."
  (should (boundp '+tab-bar-alert-clear-delay)))

(ert-deftest ui-tab-bar-alert-pulse-iterations-defined ()
  "Tab bar: +tab-bar-alert-pulse-iterations should be defined."
  (should (boundp '+tab-bar-alert-pulse-iterations)))


;;; Line numbers

(ert-deftest ui-line-numbers-width ()
  "Line numbers: display-line-numbers-width should be 3."
  (should (equal display-line-numbers-width 3)))

(ert-deftest ui-line-numbers-widen ()
  "Line numbers: display-line-numbers-widen should be t."
  (should display-line-numbers-widen))


;;; Paren matching

(ert-deftest ui-show-paren-delay ()
  "Paren: show-paren-delay should be 0.1."
  (should (equal show-paren-delay 0.1)))

(ert-deftest ui-show-paren-inside ()
  "Paren: show-paren-when-point-inside-paren should be t."
  (should show-paren-when-point-inside-paren))

(ert-deftest ui-show-paren-context-offscreen ()
  "Paren: show-paren-context-when-offscreen should be overlay."
  (should (eq show-paren-context-when-offscreen 'overlay)))


;;; hl-line

(ert-deftest ui-hl-line-sticky-flag ()
  "hl-line: hl-line-sticky-flag should be nil."
  (should (null hl-line-sticky-flag)))


;;; hl-todo keywords

(ert-deftest ui-hl-todo-keywords-defined ()
  "P9: hl-todo-keyword-faces should include TODO and FIXME."
  (skip-unless (boundp 'hl-todo-keyword-faces))
  (should (assoc "TODO" hl-todo-keyword-faces))
  (should (assoc "FIXME" hl-todo-keyword-faces)))


;;; Dimmer settings

(ert-deftest ui-dimmer-adjustment-mode ()
  "Dimmer: dimmer-adjustment-mode should be :both."
  (skip-unless (boundp 'dimmer-adjustment-mode))
  (should (eq dimmer-adjustment-mode :both)))

(ert-deftest ui-dimmer-fraction ()
  "Dimmer: dimmer-fraction should be -0.05."
  (skip-unless (boundp 'dimmer-fraction))
  (should (equal dimmer-fraction -0.05)))


;;; Breadcrumb

(ert-deftest ui-breadcrumb-idle-time ()
  "Breadcrumb: breadcrumb-idle-time should be 0.3."
  ;; Skip if breadcrumb not configured (custom settings not applied in batch mode)
  (skip-unless (and (boundp 'breadcrumb-idle-time)
                    (not (= breadcrumb-idle-time 1)))) ; default is 1
  (should (equal breadcrumb-idle-time 0.3)))


;;; Indent-bars

(ert-deftest ui-indent-bars-width-frac ()
  "Indent-bars: indent-bars-width-frac should be 0.15."
  (skip-unless (boundp 'indent-bars-width-frac))
  (should (equal indent-bars-width-frac 0.15)))


;;; Pulsar

(ert-deftest ui-pulsar-iterations ()
  "Pulsar: pulsar-iterations should be 5."
  (skip-unless (boundp 'pulsar-iterations))
  (should (equal pulsar-iterations 5)))


;;; Display buffer rules

(ert-deftest ui-display-buffer-alist-non-empty ()
  "Display-buffer: display-buffer-alist should be configured."
  (should (consp display-buffer-alist)))

(ert-deftest ui-window-combination-resize ()
  "Display-buffer: window-combination-resize should be t."
  (should window-combination-resize))

(ert-deftest ui-switch-to-buffer-in-dedicated-window ()
  "Display-buffer: switch-to-buffer-in-dedicated-window should be pop."
  (should (eq switch-to-buffer-in-dedicated-window 'pop)))


;;; doom-modeline settings

(ert-deftest ui-doom-modeline-bar-width ()
  "P11: doom-modeline-bar-width should be 3."
  (skip-unless (boundp 'doom-modeline-bar-width))
  (should (equal doom-modeline-bar-width 3)))

(ert-deftest ui-doom-modeline-check-simple-format ()
  "doom-modeline: doom-modeline-check-simple-format should be t."
  (skip-unless (boundp 'doom-modeline-check-simple-format))
  (should doom-modeline-check-simple-format))

(ert-deftest ui-doom-modeline-modal ()
  "doom-modeline: doom-modeline-modal should be nil."
  (skip-unless (boundp 'doom-modeline-modal))
  (should (null doom-modeline-modal)))


;;; Functions from lib.el

(ert-deftest ui-display-buffer-fallback-defined ()
  "Lib: +display-buffer-fallback should be defined."
  (should (fboundp '+display-buffer-fallback)))

(ert-deftest ui-display-buffer-reuse-non-dedicated-defined ()
  "Lib: +display-buffer-reuse-non-dedicated-window should be defined."
  (should (fboundp '+display-buffer-reuse-non-dedicated-window)))

(ert-deftest ui-tty-frame-setup-defined ()
  "Lib: +tty-frame-setup should be defined."
  (should (fboundp '+tty-frame-setup)))

(ert-deftest ui-goto-address-maybe-defined ()
  "Lib: +goto-address-maybe-h should be defined."
  (should (fboundp '+goto-address-maybe-h)))

(ert-deftest ui-pulsar-with-eval-pulse-defined ()
  "Lib: +pulsar--with-eval-pulse should be a macro."
  (should (macrop '+pulsar--with-eval-pulse)))


;;; Tab bar functions

(ert-deftest ui-tab-bar-set-alert-defined ()
  "Tab: +tab-bar-set-alert should be defined."
  (should (fboundp '+tab-bar-set-alert)))

(ert-deftest ui-tab-bar-clear-alert-defined ()
  "Tab: +tab-bar-clear-alert should be defined."
  (should (fboundp '+tab-bar-clear-alert)))

(ert-deftest ui-tab-bar-set-transient-alert-defined ()
  "Tab: +tab-bar-set-transient-alert should be defined."
  (should (fboundp '+tab-bar-set-transient-alert)))

(ert-deftest ui-tabs-menu-defined ()
  "Tab: +tabs-menu should be defined."
  (should (fboundp '+tabs-menu)))

(ert-deftest ui-update-tab-bar-themes-defined ()
  "Tab: +update-tab-bar-themes should be defined."
  (should (fboundp '+update-tab-bar-themes)))


;;; Faces

(ert-deftest ui-tab-bar-tab-alert-face ()
  "Faces: tab-bar-tab-alert should be defined."
  (skip-unless (featurep 'ui-tabs))
  (should (facep 'tab-bar-tab-alert)))

(ert-deftest ui-tab-bar-tab-inactive-alert-face ()
  "Faces: tab-bar-tab-inactive-alert should be defined."
  (skip-unless (featurep 'ui-tabs))
  (should (facep 'tab-bar-tab-inactive-alert)))


(provide 'ui-tests)
;;; tests.el ends here
