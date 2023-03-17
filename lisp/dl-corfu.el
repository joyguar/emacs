;;; dl-corfu.el -*- lexical-binding: t; -*-
(require 'corfu)
(require 'cape)

(defun dl/corfu-ignore-elisp-keywords (cand)
  (or (not (keywordp cand))
      (eq (char-after (car completion-in-region--data)) ?:)))

(defun dl/corfu-setup-elisp ()
  (setq-local completion-at-point-functions
              `(,(cape-super-capf
                  (cape-capf-predicate
                   #'elisp-completion-at-point
                   #'dl/corfu-ignore-elisp-keywords)
                  #'cape-dabbrev)
                cape-file)
              cape-dabbrev-min-length 4))

(provide 'dl-corfu)
;;; dl-corfu.el ends here
