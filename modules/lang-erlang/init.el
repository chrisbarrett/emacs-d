;;; init.el --- Erlang support -*- lexical-binding: t; -*-

;;; Commentary:

;; BEAM file hiding for Erlang/OTP compilation artifacts.
;; The erlang package itself is disabled (large, slow to clone).

;;; Code:

(require '+autoloads)

(require '+corelib)

;; Hide BEAM compilation artifacts from completion
(pushnew! completion-ignored-extensions ".jam" ".vee" ".beam")

;; Hide BEAM compilation artifacts in dired-omit-mode
(with-eval-after-load 'dired-x
  (pushnew! dired-omit-extensions ".jam" ".vee" ".beam"))



;;; init.el ends here
