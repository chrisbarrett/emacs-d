;;; terragrime-vars.el --- Shared variables & customization interface for terragrime. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defgroup terragrime nil
  "Magit-style interface for terragrunt."
  :group 'tools
  :prefix "terragrime-")

(defcustom terragrime-terragrunt-executable 'mise
  "Location of the terragrunt executable.

This can be:
- An absolute path to the terragrunt binary.
- A program name to be resolved via `executable-find'.
- The symbol `mise' to use the version declared via mise."
  :type '(choice (const :tag "Use mise" mise)
          (string :tag "Executable path or name"))
  :group 'terragrime)

(provide 'terragrime-vars)

;;; terragrime-vars.el ends here
