;;; lib.el --- Theme detection and switching.  -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides theme management with automatic light/dark switching based on
;; system preferences. Supports macOS and GNU/Linux.

;;; Code:

(require 'cl-lib)
(require 'color)

(defvar +theme-light nil
  "Symbol of light theme to use.")

(defvar +theme-dark nil
  "Symbol of dark theme to use.")

(defvar +theme-changed-hook nil
  "Hook run after theme changes.")

(defvar +theme-override nil
  "If set, overrides system theme detection.")

(cl-defgeneric +system-theme-query (system-type)
  "Query SYSTEM-TYPE for its dark mode preference.")

(cl-defmethod +system-theme-query ((_ (eql 'darwin)))
  "Query macOS for dark mode preference."
  (shell-command-to-string "defaults read -g AppleInterfaceStyle"))

(cl-defmethod +system-theme-query ((_ (eql 'gnu/linux)))
  "Query GNU/Linux for dark mode preference."
  (shell-command-to-string "gsettings get org.gnome.desktop.interface gtk-theme"))

(defun +theme-dark-p ()
  "Return non-nil if current theme is dark.
Detection is based on background luminance being less than 50%."
  (let ((default-bg (face-background 'default nil t)))
    (< (apply #'+ (color-values default-bg)) (* 3 32768))))

;;;###autoload
(defun +theme-for-system-theme ()
  "Return the theme symbol appropriate for current system mode.
If `+theme-override' is set, returns that instead of querying
the system."
  (or +theme-override
      (if (string-match-p "dark" (+system-theme-query system-type))
          +theme-dark
        +theme-light)))

;;;###autoload
(defun +theme-update ()
  "Sync the Emacs theme with the system preference."
  (let* ((inhibit-redisplay t)
         (updated-theme (+theme-for-system-theme)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme updated-theme t)
    (run-hooks '+theme-changed-hook)))

;;;###autoload
(defun +theme-dark ()
  "Switch to the dark theme.
Uses the theme specified in `+theme-dark'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme +theme-dark t)
  (run-hooks '+theme-changed-hook))

;;;###autoload
(defun +theme-light ()
  "Switch to the light theme.
Uses the theme specified in `+theme-light'."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme +theme-light t)
  (run-hooks '+theme-changed-hook))


