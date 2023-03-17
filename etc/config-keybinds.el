;;; config-keybinds.el -*- lexical-binding: t; -*-

;;; Builtin

(setup transient
  (:option transient-history-file (dl/emacs-data "transient/history")
           transient-values-file (dl/emacs-data "transient/values")
           transient-levels-file (dl/emacs-data "transient/levels")))

;;; External

(setup (:elpaca which-key)
  (:option which-key-side-window-location 'bottom
           which-key-sort-order 'which-key-key-order-alpha
           which-key-side-window-max-width 0.33
           which-key-idle-delay 0.25
           which-key-mode t))

(provide 'config-keybinds)
;;; config-keybinds.el ends here
