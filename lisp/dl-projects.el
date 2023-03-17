;;; dl-project.el -*- lexical-binding: t; -*-

(defcustom dl-user-projects-dir (expand-file-name "~/projects")
  "User's projects folder."
  :type 'directory
  :group 'dl)

(defcustom dl-user-src-dir (expand-file-name "~/src")
  "User's src folder."
  :type 'directory
  :group 'dl)

(provide 'dl-project)
;;; dl-projects.el ends here
