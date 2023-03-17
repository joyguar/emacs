;;; dl-flycheck.el -*- lexical-binding: t; -*-

;; Aspell

(defvar dl-emacs-aspell-bin (executable-find "aspell")
  "Spell checker program name.")

(defvar dl-aspell-dict-dir (expand-file-name "dict" dl-emacs-data-dir)
  "Local aspell dictionaries directory.")

(provide 'dl-flycheck)
;;; dl-flycheck.el ends here
