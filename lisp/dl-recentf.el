;;; dl-recentf.el -*- lexical-binding: t; -*-

(defun dl/recentf-touch-buffer-h ()
  "Bump file in recent file list when it is switched or written to."
  (when buffer-file-name
    (recentf-add-file buffer-file-name))
  nil)

(provide 'dl-recentf)
;;; dl-recentf.el ends here
