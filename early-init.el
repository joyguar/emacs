;;; early-init.el -*- lexical-binding: t; -*-
;;; Code:
;;;

;; Basic settings and Startup Optimizations

(setq load-prefer-newer t
      inhibit-default-init t)

;; Startup lib

(defvar dl/emacs--startup-restore-alist nil
  "Variables and values to restore after init.")

(add-hook 'emacs-startup-hook
          (defun emacs-startup@restore-values ()
            "Restore values set during init.
This applies values in `dl/emacs--startup-restore-alist'."
            (dolist (var dl/emacs--startup-restore-alist)
              (set (car var) (cdr var)))))

(defun dl/set-during-startup (variable value &optional restore)
  "Set VARIABLE to VALUE during startup, but restore to RESTORE.
If RESTORE is nil or not passed, save the original value and
restore that."
  (unless after-init-time
    (setf (alist-get variable dl/emacs--startup-restore-alist)
          (or restore (symbol-value variable)))
    (set-default variable value)))

;; Garbage Collection
(dl/set-during-startup 'gc-cons-threshold most-positive-fixnum (* 20 1024 1024))
(dl/set-during-startup 'gc-cons-percentage 1 0.1)
(dl/set-during-startup 'file-name-handler-alist nil)

;;; Display
(unless debug-on-error
  (dl/set-during-startup 'inhibit-redisplay t)
  (dl/set-during-startup 'inhibit-message t))

;; Debug during init
(unless (eq debug-on-error 'startup)
  (dl/set-during-startup 'debug-on-error 'init))

;; UI

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'display-startup-screen :override #'ignore)

;; X resources

(advice-add #'x-apply-session-resources :override #'ignore)

;; Byte-compile custom lisp and configuration files, setup load paths and custom lib

(dolist (path '("lisp" "etc"))
;;  (byte-recompile-directory (locate-user-emacs-file path) 0)
  (add-to-list 'load-path (locate-user-emacs-file path)))

(require 'dl)

;; Native Comp

(setq native-comp-async-report-warnings-errors nil)

(startup-redirect-eln-cache (expand-file-name "eln" dl-emacs-cache-dir))

;; Packages

(setq package-enable-at-startup nil
      package-quickstart nil)

(require 'dl-packages)

(provide 'early-init)
;;; early-init.el ends here
