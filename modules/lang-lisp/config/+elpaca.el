;;; +elpaca.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require '+corelib)
(require 'elpaca)

(setq-hook! 'emacs-config-mode-hook
  elisp-flymake-byte-compile-load-path
  (append elisp-flymake-byte-compile-load-path
          (seq-filter (lambda (path)
                        (string-prefix-p elpaca-builds-directory path))
                      load-path)))

;;; +elpaca.el ends here
