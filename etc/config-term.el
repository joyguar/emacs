;;; config-term.el ---  dired mode settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Builtin

(setup eshell
  (:option eshell-directory-name (dl/emacs-cache "eshell")
           eshell-history-file-name (expand-file-name "history" eshell-directory-name)))

(provide 'config-term)
;;; config-term.el ends here
