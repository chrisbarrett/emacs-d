;;; init.el --- Core module initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; The core module provides foundational infrastructure for the Emacs
;; configuration. Unlike other modules, most of core's functionality must
;; load before the module system itself initializes:
;;
;; - lisp/+corelib.el: Foundation utilities (loaded in early-init.el)
;; - lisp/+modules.el: Module system (loaded before modules)
;; - init/init-hooks.el: Lifecycle hooks (loaded via init.el)
;; - init/init-system.el: System integration (loaded via init.el)
;; - init/init-readonly.el: Read-only protection (loaded via init.el)
;;
;; This module's init.el is minimal because the heavy lifting happens
;; in the files above, which are loaded during Emacs' normal init sequence.
;;
;; The module exists primarily to:
;; 1. Declare package dependencies in packages.eld
;; 2. Provide a spec.md documenting core behavior
;; 3. Provide tests.el verifying core functionality

;;; Code:

;; Core init files are loaded separately during init.el execution,
;; not as part of module loading, because they must run before
;; other modules can be loaded.

(provide 'core-init)

;;; init.el ends here
