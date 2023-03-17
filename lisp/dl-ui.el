;;; dl-ui.el -*- lexical-binding: t; -*-

;; Variables

(defcustom dl-emacs-mono-font "IBM Plex Mono"
  "Default mono font."
  :type 'string
  :group 'dl)

(defcustom dl-emacs-sans-font "Inria Sans"
  "Default sans font."
  :type 'string
  :group 'dl)

(defcustom dl-emacs-serif-font "Inria Serif"
  "Default serif font."
  :type 'string
  :group 'dl)

;; Hooks

(defcustom dl-emacs-config-gui-hook nil
  "Normal hook run after frame creation is done."
  :type 'hook
  :group 'dl)

(defcustom dl-emacs-theme-enabled-hook nil
  "Normal hook run after new theme is enabled."
  :type 'hook
  :group 'dl)

(provide 'dl-ui)
;;; dl-ui.el ends here
