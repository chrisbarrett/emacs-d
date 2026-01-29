;;; init.el --- Document reader module -*- lexical-binding: t; -*-

;;; Commentary:

;; General-purpose document reader. Uses native modules with buffered
;; rendering for improved performance.
;;
;; The reader package is injected via Nix (not Elpaca) as it requires
;; native compilation.

;;; Code:

(require '+autoloads)

;; Load autoloads to register file associations and commands.
;; The package is native (compiled C/Rust) and injected via Nix.
(condition-case nil
    (require 'reader-autoloads)
  (file-missing nil))


