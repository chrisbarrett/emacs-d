;;; lib.el --- DESC -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'subr-x)


;;;###autoload
(defun +terragrunt-find-sibling-file-func (path)
  (when-let* ((tg-root (locate-dominating-file path "root.hcl"))
              (account-base (locate-dominating-file path "account.hcl"))
              (this-account (nth 1 (nreverse (split-string account-base "/"))))
              (relpath (string-remove-prefix account-base (abbreviate-file-name path)))
              (account-hcls (file-expand-wildcards (file-name-concat tg-root "*" "account.hcl")))
              (all-accounts (seq-keep (lambda (dir)
                                        (nth 1 (nreverse (split-string dir "/"))))
                                      account-hcls))
              (other-accounts (seq-difference all-accounts (list this-account))))
    (seq-map (lambda (account-name)
               (file-name-concat tg-root account-name relpath))
             other-accounts)))

;;; lib.el ends here
