;;; +bufler.el --- Configure bufler -*- lexical-binding: t; -*-

;;; Commentary:

;; `bufler' implements a buffer list using `magit-section'.
;; I don't use the workspace features--just the buffer list.

;;; Code:

(require 'bufler)

(defvar org-directory) ; org

(bufler-defauto-group nix-store-path
  (when-let* ((filename (or (buffer-file-name buffer)
                            (when (buffer-base-buffer buffer)
                              (buffer-file-name (buffer-base-buffer buffer)))))
              (store-path (pcase (file-name-split filename)
                            (`("" "nix" "store" ,store-path . ,_)
                             store-path)))
              (hash-delimiter (string-match "-" store-path))
              (name (substring store-path (1+ hash-delimiter))))
    (concat name)))

(setf bufler-groups
      (bufler-defgroups
        (group (auto-workspace))

        (group
         (group-or "*Help/Info*"
                   (mode-match "*Help*" (rx bos "help-"))
                   (mode-match "*Info*" (rx bos "info-"))))

        (group
         (dir user-emacs-directory))

        (group
         (dir org-directory)
         (group
          (auto-indirect)
          (auto-file))
         (group-not "*special*" (auto-file)))

        (group
         (auto-parent-project)
         (group-not "special"
                    (group-or "Non-file-backed and neither Dired nor Magit"
                              (mode-match "Magit Status" (rx bos "magit-status"))
                              (mode-match "Dired" (rx bos "dired-"))
                              (auto-file))))

        (group
         (lambda (buf)
           (when (and (buffer-file-name buf)
                      (string-prefix-p "/nix/store" (buffer-file-name buf)))
             "/nix/store"))
         (bufler-group 'auto-nix-store-path))

        (group
         (group-not "*Special"
                    (group-or "*Special*"
                              (mode-match "Magit" (rx bos "magit-"))
                              (mode-match "Forge" (rx bos "forge-"))
                              (mode-match "Dired" (rx bos "dired"))
                              (mode-match "grep" (rx bos "grep-"))
                              (mode-match "compilation" (rx bos "compilation-"))
                              (auto-file)))
         (group
          (name-match "**Special**"
                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
         (group
          (mode-match "*Magit* (non-status)" (rx bos "magit-"))
          (auto-directory))

         (auto-mode))

        (auto-directory)
        (auto-mode)))

;;; +bufler.el ends here
