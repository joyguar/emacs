;;; config-ui.el -*- lexical-binding: t; -*-

;;; C defined features

(setup xdispc
  (:option scroll-conservatively 1000
           scroll-margin 0
           scroll-preserve-screen-position t
           scroll-step 1
           display-line-numbers-width 3
           x-stretch-cursor t
           highlight-nonselected-windows nil))

(setup windowc
  (:option auto-window-vscroll nil))

(setup minibufc
  (:option enable-recursive-minibuffers t
           minibuffer-prompt-properties '( read-only t
                                           intangible t
                                           cursor-intangible t
                                           face minibuffer-prompt)))

(setup terminalc
  (:option ring-bell-function #'ignore))

(setup termc
  (:option visible-cursor nil))

(setup fnsc
  (:option use-dialog-box nil
           use-short-answers t))

(setup dispnewc
  (:option visible-bell nil))

;;; Builtin

(setup modes
  (:option menu-bar-mode nil
           tool-bar-mode nil
           scroll-bar-mode nil
           tooltip-mode nil
           blink-cursor-mode nil))

(setup frame
  (:option window-divider-default-places t
           window-divider-default-bottom-width 1
           window-divider-default-right-width 1))

(setup window
  (:option split-width-threshold 160))

(setup mouse
  (:option mouse-yank-at-point t ; middle-click paste at point, not at click
           mouse-drag-and-drop-region t
           mouse-drag-and-drop-region-cross-program t))

(setup pgtk
  (:option x-gtk-use-system-tooltips nil))

(setup bindings
  (:option mode-line-default-help-echo nil))

(setup ediff
  (:option ediff-split-window-function 'split-window-horizontally
           ediff-window-setup-function 'ediff-setup-windows-plain))

(setup hl-line
  (:option hl-line-range-function #'dl/hl-line-range
           hl-line-sticky-flag nil)
  (:hook-into conf-mode dired-mode prog-mode text-mode))

(setup winner
  (:option winner-dont-bind-my-keys t
           winner-mode t))

(setup paren
  (:option show-paren-delay 0.1
           show-paren-highlight-openparen t
           show-paren-when-point-inside-paren t
           show-paren-when-point-in-periphery t
           show-paren-mode t))

(setup whitespace
  (:option whitespace-line-column nil
           whitespace-style '(face trailing tabs tab-mark empty))
  ;; (:face 'whitespace-trailing '((t :inherit nil)))
  (:hook-into text-mode prog-mode conf-mode))

(setup uniquify
  (:option uniquify-buffer-name-style 'forward))

;;; External

(setup (:require dl-minibuffer)
  (:long-hook minibuffer-setup-hook #'garbage-collect@minibuffer-enter
             minibuffer-exit-hook #'garbage-collect@minibuffer-exit))

(setup (:elpaca svg-lib)
  (:option svg-lib-icons-dir (dl/emacs-cache "svg-lib/")))

(setup (:elpaca all-the-icons))

(setup (:elpaca rainbow-delimiters)
  (:option rainbow-delimiters-max-face-count 5)
  (:hook-into prog-mode-hook))

(setup (:elpaca highlight-numbers)
  (:option highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>")
  (:hook-into prog-mode conf-mode))

(setup (:elpaca doom-themes)
  (:option doom-themes-enable-bold t
           doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-one t))

(setup (:elpaca doom-modeline)
  (:option doom-modeline-height 15
           doom-modeline-bar-width 6
           doom-modeline-minor-modes t
           doom-modeline-file-name-style 'relative-from-project
           doom-modeline-mode t))

(setup (:elpaca solaire-mode)
  (:option solaire-global-mode t))

(setup (:elpaca visual-fill-column)
  (:hook-into org-mode))

(provide 'config-ui)
;;; config-ui.el ends here
