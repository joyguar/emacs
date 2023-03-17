;;; dl-minibuffer.el ---  compilation settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; hooks

(defun garbage-collect@minibuffer-enter ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun garbage-collect@minibuffer-exit ()
  (setq gc-cons-threshold (* 20 1024 1024)))

(provide 'dl-minibuffer)
;;; dl-minibuffer.el ends here
