;;; dl-org.el --- org functionalities. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(require 'dl)

;;; Variables

(defgroup dl-org nil
  "Custom variables and library for org."
  :group 'org)

(defcustom dl-org-root-dir (expand-file-name "org" dl-user-home-dir)
  "Dl-Org root folder."
  :type 'directory
  :group 'dl-org)

(defcustom dl-org-gtd-dir (expand-file-name "gtd"
                                            dl-org-root-dir)
  "Dl-Org Getting Things Done folder."
  :type 'directory
  :group 'dl-org)

(defcustom dl-org-pkm-dir (expand-file-name "pkm"
                                            dl-org-root-dir)
  "Dl-Org Personal Knowledge Management folder."
  :type 'directory
  :group 'dl-org)

(defcustom dl-org-refs-dir (expand-file-name "refs"
                                             dl-org-root-dir)
  "Dl-Org references folder."
  :type 'directory
  :group 'dl-org)

(defcustom dl-org-writings-dir (expand-file-name "writings"
                                                 dl-org-root-dir)
  "Dl-Org writings folder."
  :type 'directory
  :group 'dl-org)

(defcustom dl-org-id-file (expand-file-name ".orgids"
                                            dl-org-root-dir)
  "Dl-Org id locations file."
  :type 'file
  :group 'dl-org)

(defcustom dl-org-inbox-file (expand-file-name "inbox.org"
                                               dl-org-gtd-dir)
  "Dl-Org inbox file."
  :type 'file
  :group 'dl-org)

(defcustom dl-org-slipbox-file (expand-file-name "slipbox.org"
                                                 dl-org-pkm-dir)
  "Dl-Org slipbox file."
  :type 'file
  :group 'dl-org)


;;; Library

;;;###autoload
(defun dl/org-insert-heading-todo-created-h ()
  (let ((cmnd this-command)
        (hdngs '(org-insert-todo-heading-respect-content org-insert-todo-subheading)))
    (when (memq cmnd hdngs)
      (insert (format-time-string
               (concat "\nCREATED: "
                       (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]"))))
      (org-back-to-heading)
      (move-end-of-line()))))

;;;###autoload
(defun dl/org-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
               "."
               (seq-map
                (lambda (p)
                  (string-remove-prefix dl-org-directory p))
                (directory-subdirs dl-org-directory 'recursive)))))
    (concat dl-org-directory (completing-read "Subdir: " dirs nil t))))

(provide 'dl-org)
;;; dl-org.el ends here
