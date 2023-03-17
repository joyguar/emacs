;;; config-core.el -*- lexical-binding: t; -*-

;;; C defined features

(setup evalc
  (:option debug-on-error t))

(setup keyboardc
  (:option echo-keystrokes 0.10))

(setup minibufc
  (:option history-length 200))

;;; Builtin

(setup compile
  (:option compilation-always-kill t
           compilation-ask-about-save nil
           compilation-scroll-output 'first-error)
  (:long-hook compilation-filter-hook #'dl/compile-colorize))

(setup indent
  (:option tab-always-indent 'complete))

(setup register
  (:option register-preview-delay 0.5))

(setup minibuffer
  (:option completion-styles '(basic partial-completion)
           completion-category-defaults nil))

(setup advice
  (:option ad-redefinition-action 'accept))

(setup auth-source
  (:option auth-sources (list (dl/emacs-state "authinfo.gpg")
                              "~/.authinfo.gpg")))

(setup cus-edit
  (:option custom-file (dl/emacs-user "set-custom.el"))
  (:advise #'en/disable-command :around #'en/disable-command@write-to-data-dir))

(setup encodings
  (set-language-environment "UTF-8"))

(provide 'config-core)
;;; config-core.el ends here
