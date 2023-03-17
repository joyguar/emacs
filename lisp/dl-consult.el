;;; dl-consult.el --- Description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dl/consult-ripgrep-dir-select ()
    "Query the user for a directory to ripgrep search in."
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively 'consult-ripgrep)))

(provide 'dl-consult)
;;; dl-consult.el ends here
