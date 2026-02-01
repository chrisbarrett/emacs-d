;;; +loads.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require '+corelib)
(require '+modules)

;; Add lisp/ dir to load path in init files.

(add-hook! 'emacs-config-mode-hook
  (pushnew! elisp-flymake-byte-compile-load-path (expand-file-name +lisp-dir)))

;; Regenerate autoloads when saving lib files in modules/

(defun +elisp-maybe-regenerate-autoloads-h ()
  "Regenerate module autoloads if current file is a module lib file."
  (when-let* ((file (buffer-file-name))
              (modules-dir (file-name-concat user-emacs-directory "modules/"))
              ((string-prefix-p modules-dir file))
              ((or (string-suffix-p "/lib.el" file)
                   (string-match-p "/lib/[^/]+\\.el\\'" file))))
    (+modules-regenerate-autoloads)))

(add-hook! 'emacs-config-mode-hook
  (add-hook 'after-save-hook #'+elisp-maybe-regenerate-autoloads-h nil t))

;;; +loads.el ends here
