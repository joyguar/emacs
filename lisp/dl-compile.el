;;; dl-compile.el ---  compilation settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun dl/compile-colorize ()
  "Colorize a compilation mode buffer."
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (require 'ansi-color)
      (ansi-color-apply-on-region (point-min) (point-max)))))

(provide 'dl-compile)
;;; dl-compile.el ends here
