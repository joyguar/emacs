;;; config-editor.el -*- lexical-binding: t; -*-

;;; C defined features

(setup filelockc
  (:option create-lockfiles nil))

(setup bufferc
  (:option cursor-in-non-selected-windows nil
           fill-column 80
           tab-width 4
           bidi-display-reordering nil
           buffer-file-coding-system 'utf-8
           mode-line-format nil))

;;; Builtin

(setup simple
  (:option indent-tabs-mode nil
           kill-do-not-save-duplicates t)
  (:long-hook org-mode-hook #'visual-line-mode))

(setup files
  (:option find-file-visit-truename t
           find-file-suppress-same-file-warnings t
           confirm-nonexistent-file-or-buffer nil
           make-backup-files nil
           auto-save-default t
           auto-save-include-big-deletions t
           auto-save-list-file-prefix (dl/emacs-cache "autosave/")
           auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
           delete-old-versions t
           kept-new-versions 5
           kept-old-versions 5
           require-final-newline t
           remote-file-name-inhibit-cache 60)
  (:long-hook find-file-not-found-functions #'dl/auto-create-missing-dirs))

(setup savehist
  (:option savehist-save-minibuffer-history t
           savehist-autosave-interval 60
           savehist-file (dl/emacs-cache "savehist")
           savehist-additional-variables '(kill-ring
                                           register-alist
                                           mark-ring global-mark-ring
                                           search-ring regexp-search-ring)))

(setup autorevert
  (:option auto-revert-verbose t
           auto-revert-use-notify nil
           auto-revert-stop-on-user-input nil
           revert-without-query (list ".")
           global-auto-revert-mode t))

(setup saveplace
  (:option save-place-file (dl/emacs-cache "saveplace")
           save-place-mode t))

(setup bookmark
  (:option bookmark-default-file (dl/emacs-cache "bookmarks")
           bookmarks-save-flag t))

(setup recentf
  (:require recentf dl-recentf)
  (:option recentf-auto-cleanup nil
           recentf-save-file (dl/emacs-cache "recentf")
           recentf-max-saved-items 100
           recentf-max-menu-items 100
           recentf-mode t)
  (:add-to-list recentf-exclude (list "^/sudo:" "^/tmp/" "^/ssh:"
                                      "/TAGS$" "^/var/folders/.+$"
                                      (concat "^" dl-emacs-cache-dir)
                                      (concat "^" dl-xdg-runtime-dir))
                recentf-filename-handlers (list #'file-truename
                                                #'substring-no-properties))
  (:long-hook write-file-functions #'dl/recentf-touch-buffer-h
              kill-emacs-hook #'recentf-cleanup))

(setup tramp
  (:option tramp-verbose 1
           tramp-default-method "ssh"
           tramp-persistency-file-name (dl/emacs-cache "tramp")))

(setup apropos
  (:option apropos-do-all t))

(setup find-func
  (:option find-function-C-source-directory "~/src/emacs-src/29.0.6/src/"))

(setup help
  (:option help-window-select t))

(setup executable
  (:long-hook after-save-hook #'executable-make-buffer-file-executable-if-script-p))

(setup paren
  (:option show-paren-delay 0.1
           show-paren-highlight-openparen t
           show-paren-when-point-in-periphery t
           show-paren-when-point-inside-paren t))

;;; External

(setup (:elpaca helpful)
  (:require helpful))

(provide 'config-editor)
;;; config-editor.el ends here
