;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
;;(setq package-enable-at-startup nil)
;;(fset #'package--ensure-init-file #'ignore)  ; DEPRECATED Removed in 28

;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

(add-to-list 'load-path "~/.config/emacs/modules")

(setq byte-compile-warnings '(not obsolete))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)


(make-thread (require 'optimizations))
