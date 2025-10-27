;;; mod-tabs.el --- Tab management with transient menu -*- lexical-binding: t; -*-

;;; Commentary:

;; This module provides a transient menu for tab management operations.

;;; Code:

(require 'tab-bar)
(require 'transient)

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
    ("o" "Close other tabs" tab-bar-close-other-tabs)]])

(provide 'mod-tabs)

;;; mod-tabs.el ends here
