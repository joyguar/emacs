;;; config-completion.el -*- lexical-binding: t; -*-

;;; External

(setup abbrev
  (:option abbrev-file-name (dl/emacs-cache "abbrev.el"))
  (:hook-into text-mode-hook))

(setup (:elpaca orderless)
  (:option completion-styles '(orderless basic)))

(setup (:elpaca vertico)
  (:option vertico-resize nil
           vertico-count 17
           vertico-cycle t
           vertico-mode t))

(setup (:elpaca marginalia)
  (:option marginalia-mode t))

(setup (:elpaca embark)
  (:option prefix-help-command #'embark-prefix-help-command
           embark-prompter #'embark-completing-read-prompter)
  (:global "C-." embark-act
           "C-;" embark-dwim
           "C-h B" embark-bindings)
  (add-to-list 'display-buffer-alist '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                       nil
                                       (window-parameters (mode-line-format . none)))))

(setup (:elpaca consult)
  (:require dl-consult)
  (:option consult-narrow-key "<"
           consult-line-numbers-widen t
           consult-async-min-input 2
           consult-async-refresh-delay 0.15
           consult-async-input-throttle 0.2
           consult-async-input-debounce 0.1
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)
  (:global [remap man] #'consult-man
           [remap bookmark-jump] #'consult-bookmark
           [remap goto-lines] #'consult-goto-line
           [remap imenu] #'consult-imenu
           [remap locate] #'consult-locate
           [remap load-theme] #'consult-theme
           [remap recentf-open-files] #'consult-recent-file
           [remap switch-to-buffer] #'consult-buffer
           [remap switch-to-buffer-other-window] #'consult-buffer-other-window
           [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame
           [remap yank-pop] #'consult-yank-pop
           "C-x /" consult-ripgrep
           "C-x ?" dl/consult-ripgrep-dir-select
           "C-h M" consult-man
           "C-h I" consult-info)
  (:when-loaded (consult-customize consult-ripgrep
                                   consult-git-grep
                                   consult-grep
                                   consult-bookmark
                                   consult-recent-file
                                   consult--source-buffer
                                   consult-xref
                                   consult--source-recent-file
                                   consult--source-bookmark
                                   consult--source-project-buffer
                                   :preview-key 'any)))

(setup (:elpaca consult-dir)
  (:global [remap list-directory] #'consult-dir))

(setup (:elpaca consult-org-roam)
  (:option consult-org-roam-grep-func #'consult-ripgrep
           consult-org-roam-buffer-narrow-key ?r
           consult-org-roam-buffer-after-buffers nil
           consult-org-roam-mode t))

(setup (:elpaca embark-consult)
  (:long-hook embark-collect-mode-hook #'consult-preview-at-point-mode))

(setup (:elpaca corfu)
  (:require dl-corfu)
  (:option corfu-cycle t
           corfu-popup-info-delay 0.2
           corfu-history-mode t
           corfu-popup-info-mode t
           global-corfu-mode t)
  (:long-hook emacs-lisp-mode-hook dl/corfu-setup-elisp)
  (keymap-set corfu-map "SPC" #'corfu-insert-separator))

(setup (:elpaca cape)
  (:add-to-list completion-at-point-functions (list #'cape-dabbrev
                                                     #'cape-file
                                                     #'cape-history
                                                     #'cape-tex
                                                     #'cape-ispell)))

(setup (:elpaca kind-icon)
  (:load-after corfu)
  (:option kind-icon-use-icons t
           kind-icon-default-face 'corfu-default
           kind-icon-blend-background t
           kind-icon-blend-frac 0.08)
  (:add-to-list corfu-margin-formatters #'kind-icon-margin-formatter)
  (:long-hook dl-emacs-theme-enabled-hook #'kind-icon-reset-cache))

(provide 'config-completion)
;;; config-completion.el ends here
