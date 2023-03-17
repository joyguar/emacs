;;; dl-directory.el --- directory functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun directory-subdirs (directory &optional rec)
  "Return subdirs or files of DIRECTORY.
If REC is non-nil then do recursive search."
  (let ((res
         (seq-map
          #'file-name-as-directory
          (seq-remove
           (lambda (file)
             (or (string-match "\\`\\."
                               (file-name-nondirectory file))
                 (string-match "\\`#.*#\\'"
                               (file-name-nondirectory file))
                 (string-match "~\\'"
                               (file-name-nondirectory file))
                 (not (file-directory-p file))))
           (directory-files directory t)))))
    (if rec
        (apply
         #'append
         (seq-map (lambda (p) (cons p (directory-subdirs p)))
                  res))
      res)))

(provide 'dl-directory)
;;; dl-directory.el ends here
