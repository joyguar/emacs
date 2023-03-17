;;; config-diagrams.el -*- lexical-binding: t; -*-

(setup (:elpaca d2-mode)
  (:option d2-output-format ".svg")
  (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode)) ;; add this list in whole, not element-wise
  )

(provide 'config-diagrams)
;;; config-diagrams.el ends here
